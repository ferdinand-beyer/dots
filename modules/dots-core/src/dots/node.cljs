(ns dots.node
  (:require ["typescript" :as ts]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [goog.object :as gobj]))

;; This will return 'FirstStatement' for 'VariableDeclaration'
(defn kind->str [kind]
  (gobj/get ts/SyntaxKind kind))

(defn dump-node
  ([node] (dump-node node ""))
  ([node indent]
   (println (str indent (kind->str (j/get node :kind)))
            (or (j/get node :text)
                (j/get-in node [:name :text])))))

(defn dump-tree
  ([node]
   (dump-tree node nil))
  ([node max-depth]
   (dump-tree node 0 max-depth))
  ([node depth max-depth]
   (dump-node node (str/join (repeat depth "  ")))
   (when (or (nil? max-depth) (< depth max-depth))
     (j/call node :forEachChild #(dump-tree % (inc depth) max-depth)))))

(defn path [node]
  (->> (iterate #(j/get % :parent) node)
       (take-while some?)
       reverse))

(defn dump-path [node]
  (run! dump-node (path node)))

(defn node-seq [node]
  (tree-seq #(pos? (j/call % :getChildCount)) #(j/call % :getChildren) node))
