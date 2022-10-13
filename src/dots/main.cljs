(ns dots.main
  (:require ["fs" :as fs]
            ["typescript" :as ts]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [goog.object :as gobj]))

;; This will return 'FirstStatement' for 'VariableDeclaration'
(defn- kind->str [kind]
  (gobj/get ts/SyntaxKind kind))

(defn- slurp [path]
  (.toString (fs/readFileSync path)))

(defn- parse [filepath]
  (ts/createSourceFile filepath (slurp filepath) (j/get ts/ScriptTarget :ESNext) true))

(defn- dump-node
  ([node] (dump-node node ""))
  ([node indent]
   (println (str indent (kind->str (j/get node :kind)))
            (j/get-in node [:name :text]))))

(defn- dump-tree
  ([node]
   (dump-tree node nil))
  ([node max-depth]
   (dump-tree node 0 max-depth))
  ([node depth max-depth]
   (dump-node node (str/join (repeat depth "  ")))
   (when (or (nil? max-depth) (< depth max-depth))
     (j/call node :forEachChild #(dump-tree % (inc depth) max-depth)))))

;; Also available as the undocumented `jsDoc` property
(defn- jsdoc-nodes [node]
  (->> (j/call node :getChildren)
       (filter ts/isJSDoc)))

(defn- doc-text [node]
  (->> (jsdoc-nodes node)
       (map #(ts/getTextOfJSDocComment (j/get % :comment)))
       str/join))

(defmulti walk (fn [_ctx node] (j/get node :kind)))

(defn- walk-children [ctx node]
  (j/call node :forEachChild (partial walk ctx)))

(defmethod walk :default [_ _])

(defmethod walk (j/get ts/SyntaxKind :SourceFile)
  [ctx node]
  (doseq [stmt (j/get node :statements)]
    (walk ctx stmt)))

(defmethod walk (j/get ts/SyntaxKind :ModuleDeclaration)
  [ctx node]
  (let [name (j/get-in node [:name :text])]
    (println "Module:" ctx name)
    (when-let [body (j/get node :body)]
      (walk (update ctx :module conj name) body))))

(defmethod walk (j/get ts/SyntaxKind :ModuleBlock)
  [ctx node]
  (doseq [stmt (j/get node :statements)]
    (walk ctx stmt)))

(defmethod walk (j/get ts/SyntaxKind :InterfaceDeclaration)
  [ctx node]
  (let [name (j/get-in node [:name :text])]
    (println "----- INTERFACE:" name "-----")
    (println "Doc:" (doc-text node))
    (doseq [member (j/get node :members)]
      (walk ctx member))))

(defn path [node]
  (->> (iterate #(j/get % :parent) node)
       (take-while some?)
       reverse))

(defn dump-path [node]
  (run! dump-node (path node)))

(defn node-seq [node]
  (tree-seq #(pos? (j/call % :getChildCount)) #(j/call % :getChildren) node))

(defn main []
  (let [source-file (parse "examples/vscode.d.ts")]
    (walk {:module []
           :namespace []}
          source-file)))

(comment
  (println "\n\n")

  (->> (parse "examples/vscode.d.ts")
       node-seq
       (filter ts/isFunctionDeclaration)
       first
       dump-path)

  (main)
  )
