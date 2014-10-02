;;; helm-clojure.el --- helm plugin for expore Clojure environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Andrew Rudenko

;; Author: Andrew Rudenko <ceo@prepor.ru>
;; Keywords: tools
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; https://github.com/prepor/helm-clojure

;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'cider)
(require 's)
(require 'dash)

(defun helm--get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defvar helm-clojure--file load-file-name)

(defun helm-clojure-inject ()
  (let* ((res (nrepl-send-string-sync "(find-ns 'helm-clojure)")))
    (when (equal "nil" (nrepl-dict-get res "value"))
      (nrepl-send-string-sync
       (helm--get-string-from-file
        (expand-file-name "helm-clojure.clj" (file-name-directory
                                              (or helm-clojure--file buffer-file-name))))))))

(defvar helm-clojure-f-width 45)
(defvar helm-clojure-persistent-primary t)

(defvar helm-clojure--tmp-marker (make-marker))

(defun helm-clojure-display (m)
  (let* ((str (concat (plist-get m :ns) " / " (plist-get m :symbol)))
         (sym (if (< helm-clojure-f-width (length str))
                  (helm-substring str helm-clojure-f-width)
                str))
         (width (with-helm-window (window-body-width)))
         (sep (make-string (- (+ helm-clojure-f-width 2) (string-width sym)) ? ))
         (res (-if-let (doc (plist-get m :doc))
                  (concat sym sep
                          (helm-substring-by-width doc (- width (+ helm-clojure-f-width 6))))
                sym)))
    (cons res m)))

(defun helm-clojure-candidates* ()
  (let* ((code (concat "(helm-clojure/candidates \""
                       (replace-regexp-in-string "[\\\"]" "\\\\\\&" helm-pattern)
                       "\")"))
         (res (nrepl-send-string-sync code))
         (cands (car (read-from-string (nrepl-dict-get res "value")))))
    (-map 'helm-clojure-display cands)))

(defun helm-clojure-candidates ()
  (with-helm-current-buffer
    (helm-clojure-inject)
    (helm-clojure-candidates*)))

(defun helm-clojure-init ()
  (let ((data (helm-clojure-candidates)))
    (helm-init-candidates-in-buffer
        (nrepl-current-tooling-session) data)))

(defun helm-clojure-transformer (candidate)
  (concat candidate " OK!"))

(defun helm-clojure-candidate-to-sym (c)
  (concat (plist-get c :ns) "/" (plist-get c :symbol)))

(defun helm-clojure-jump (c)
  (cider-jump-to-var (helm-clojure-candidate-to-sym c))
  (helm-highlight-current-line))

(defun helm-clojure-doc (c)
  (cider-doc-lookup (helm-clojure-candidate-to-sym c)))

(defun helm-clojure-persistent (c)
  (with-helm-alive-p
    (if (or (and (not helm-clojure-persistent-primary) (not current-prefix-arg))
            (and helm-clojure-persistent-primary current-prefix-arg))
        (helm-clojure-doc c)
      (helm-clojure-jump c))))

(defun helm-clojure--goto-ns ()
  (goto-char (point-min))
  (sp-up-sexp))

(defun helm-clojure--search-forward-within-sexp (s)
  "Searches forward for S in the current sexp.

if SAVE-EXCURSION is T POINT does not move."
  (let ((bound (save-excursion (forward-list 1) (point))))
    (search-forward s bound t)))

(defun helm-clojure--insert-in-ns (type)
  (helm-clojure--goto-ns)
  (if (helm-clojure--search-forward-within-sexp (concat "(" type))
      (if (looking-at " *)")
          (progn
            (search-backward "(")
            (forward-list 1)
            (forward-char -1)
            (insert " "))
        (search-backward "(")
        (forward-list 1)
        (forward-char -1)
        (newline-and-indent))
    (forward-list 1)
    (forward-char -1)
    (newline-and-indent)
    (insert "(" type " )")
    (forward-char -1)))


(defun helm-clojure--extract-sexp ()
  (let* ((beg (point))
         (end (progn (sp-forward-sexp)
                     (point))))
    (buffer-substring beg end)))

(defun helm-clojure-vector-part-content (part)
  (s-trim (replace-regexp-in-string "\\[?]?" "" part)))

(defun helm-clojure-ns-sexps (type)
  (save-excursion
    (helm-clojure--goto-ns)
    (if (not (helm-clojure--search-forward-within-sexp (concat "(" type)))
        nil
      (let (statements)
        (while (not (looking-at " *)"))
          (push (helm-clojure--extract-sexp) statements))
        statements))))

(defun helm-clojure-require-struct (req)
  (let* ((l (split-string req))
         (refer-index (-elem-index ":refer" l))
         (as-index (-elem-index ":as" l))
         (struct (list (cons :ns (helm-clojure-vector-part-content (car l))))))
    (when as-index
      (push (cons :alias (helm-clojure-vector-part-content (nth (1+ as-index) l))) struct))
    (when refer-index
      (let ((refer (-map
                    'helm-clojure-vector-part-content
                    (split-string (nth (1+ refer-index) l)))))
        (push (cons :refer (if (equal ":all" (car refer))
                               :all
                             refer))
              struct)))
    struct))

(defun helm-clojure-requires-struct ()
  (let* ((requires (helm-clojure-ns-sexps ":require"))
         (structs (-map 'helm-clojure-require-struct requires)))
    (--reduce-from (cons (cons (cdr (assoc :ns it)) it) acc) '() structs)))

(defun helm-clojure-alias-of-ns (ns)
  (let* ((requires (helm-clojure-requires-struct))
         (ns-require (cdr (assoc ns requires))))
    (when ns-require
      (cdr (assoc :alias ns-require)))))

(defun helm-clojure-is-in-refer-sym (ns symbol)
  (let* ((requires (helm-clojure-requires-struct))
         (ns-require (cdr (assoc ns requires)))
         (refers (cdr (assoc :refer ns-require))))
    (when ns-require
      (or (equal :all refers)
          (-elem-index symbol refers)))))

(defun helm-clojure-is-in-refer (c)
  (helm-clojure-is-in-refer-sym (plist-get c :ns) (plist-get c :symbol)))

(defun helm-clojure-alias (c)
  (helm-clojure-alias-of-ns (plist-get c :ns)))

(defun helm-clojure-insert (c)
  (cond ((helm-clojure-is-in-refer c)
         (insert (plist-get c :symbol)))
        ((helm-clojure-alias c)
         (insert (concat (helm-clojure-alias c) "/" (plist-get c :symbol))))
        (t
         (set-marker helm-clojure--tmp-marker (point))
         (helm-clojure--insert-in-ns ":require")
         (yas-expand-snippet (format "[%s :as $1]$0" (plist-get c :ns)))
         (add-hook 'yas/after-exit-snippet-hook
                   (lambda ()
                     (goto-char helm-clojure--tmp-marker)
                     (set-marker helm-clojure--tmp-marker nil)
                     (insert (concat (helm-clojure-alias c) "/" (plist-get c :symbol))))
                   nil t))))

(defvar helm-clojure-persistent-help1
  "Go to definition / C-u \\[helm-execute-persistent-action]: Show doc")

(defvar helm-clojure-persistent-help2
  "Show doc / C-u \\[helm-execute-persistent-action]: Go to definition")

(defun helm-clojure-toggle-persistent ()
  (interactive)
  (if helm-clojure-persistent-primary
      (progn
        (setq helm-clojure-persistent-primary nil)
        (helm-attrset 'persistent-help helm-clojure-persistent-help2))
    (progn
      (setq helm-clojure-persistent-primary t)
      (helm-attrset 'persistent-help helm-clojure-persistent-help1)))
  (helm-update))

(defvar helm-clojure-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c d") 'helm-clojure-toggle-persistent)
    map))

(defvar helm-source-clojure
  `((name . "Clojure")
    (init . helm-clojure-inject)
    (candidates . helm-clojure-candidates)
    (action . (("Insert" . helm-clojure-insert)
               ("Show doc" . helm-clojure-doc)
               ("Go to definition" . helm-clojure-jump)))
    (requires-pattern . 1)
    (persistent-action . helm-clojure-persistent)
    (persistent-help . ,helm-clojure-persistent-help1)
    (volatile . t)
    (keymap . ,helm-clojure-map)))

;; - TODO check jump backs
;;;###autoload
(defun helm-clojure ()
  "`helm' mode for clojure based on `cider'."
  (interactive)
  (helm :sources 'helm-source-clojure
        :buffer "*helm clojure*"))

(provide 'helm-clojure)
;;; helm-clojure.el ends here
