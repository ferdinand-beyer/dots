(ns dots.emit
  (:require [dots.util.io :as io]
            [dots.node.path :as path]
            [clojure.string :as str]
            [dots.node.fs :as fs]))

(defn- emit-def [data])

(defn- emit-defn [data])

(defn- emit-var
  [{:keys [var-name kind alias-excludes] :as data}]
  ;; - (clj-excluded? x) -> need fqn cljs.core/x
  (case kind
    :def  (emit-def data)
    :defn (emit-defn data)))

(defn- emit-ns-form [data]
  ;; - docstring
  ;; - :refer-clojure :exclude var names
  ;; - :requires
  )

(defn- namespace-munge [ns]
  (str/replace (str ns) \- \_))

(defn- ns-filepath [out-dir ns-path]
  (let [segments (map namespace-munge ns-path)
        dirs     (butlast segments)
        filename (str (last segments) ".cljs")]
    (path/join (apply path/join out-dir dirs) filename)))

(defn- emit-namespace
  [out-dir {:keys [ns-path vars] :as data}]
  (let [file-path (ns-filepath out-dir ns-path)]
    (fs/mkdir-sync (path/dirname file-path) #js {:recursive true})
    (io/with-open [writer (io/file-writer file-path)]
      (io/with-out writer
        (emit-ns-form data)
        (run! emit-var vars)))))

(defn- emit-namespaces
  [{:keys [namespaces]}]
  ;;
  )

(defn emit-project
  [out-dir namespaces]
  ;; deps.edn file
  ;; src directory
  ;; src/deps.cljs file w/ :npm-deps (?)
  ;; package.json file (?)
  )
