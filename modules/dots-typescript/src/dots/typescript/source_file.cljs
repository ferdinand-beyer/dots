(ns dots.typescript.source-file)

(defn statements
  ^"NodeArray<Statement>" [^SourceFile source-file]
  (.-statements source-file))
