(ns helm-clojure
  (:require [clojure.string :as str]))

(defprotocol Emacs
  (->emacs [v]))

(extend-protocol Emacs
  Object
  (->emacs [v]
    v)

  clojure.lang.APersistentMap
  (->emacs [m]
    (mapcat #(list (->emacs (key %)) (->emacs (val %))) m))

  clojure.lang.Sequential
  (->emacs [coll]
    (map ->emacs coll))

  nil
  (->emacs [_]
    nil))


(defn candidates
  [pattern]
  (let [regexps (map re-pattern (str/split pattern #" "))
        candidates (for [ns (all-ns)
                         [_k var] (ns-publics ns)
                         :let [m (meta var)]
                         :when (every? #(re-find % (str (name (ns-name (:ns m))) "/"
                                                        (name (:name m))))
                                       regexps)]
                     {:ns (name (ns-name (:ns m)))
                      :symbol (name (:name m))
                      :file (:file m)
                      :line (:line m)
                      :doc (when (:doc m) (str/join (str/split-lines (:doc m))))})]
    (->emacs (take 50 candidates))))
