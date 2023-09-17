(ns dots.typescript.program)

(defn get-source-file
  ^"SourceFile | undefined" [^Program program file-name]
  (.getSourceFile program file-name))

(defn get-type-checker
  ^TypeChecker [^Program program]
  (.getTypeChecker program))
