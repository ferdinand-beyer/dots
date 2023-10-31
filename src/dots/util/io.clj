(ns dots.util.io
  (:refer-clojure :exclude [with-open]))

(defmacro with-open
  [bindings & body]
  {:pre [(vector? bindings)
         (even? (count bindings))]}
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (-close ~(bindings 0)))))
    :else (throw (IllegalArgumentException.
                  "with-open only allows Symbols in bindings"))))

(defmacro with-out
  [writer & body]
  `(binding [cljs.core/*print-newline* true
             cljs.core/*print-fn* (fn [x#] (cljs.core/-write ~writer x#))]
     ~@body))
