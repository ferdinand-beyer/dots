(ns dots.typescript.namespace-import
  (:refer-clojure :exclude [name]))

(defn name
  ^Identifier [^NamespaceImport namespace-import]
  (.-name namespace-import))
