(ns dots.emit
  (:require [clojure.string :as str]
            [dots.node.fs :as fs]
            [dots.node.path :as path]
            [dots.util.io :as io]))

(defn- emit-var
  [{:keys [var-name doc arities kind alias-excludes] :as data}]
  ;; - (clj-excluded? x) -> need fqn cljs.core/x
  (println)
  (println "(defn" var-name)
  (when doc
    (print "  \"")
    (-> doc
        (str/replace "\\" "\\\\")
        (str/replace "\"" "\\\"")
        (str/replace "\n" "\n  ")
        print)
    (println "\""))
  (print "  {:arglists '(")
  (doseq [{:keys [arglists]} (vals arities)
          args arglists]
    (print "[")
    (print (str/join " " args))
    (print "] "))
  (println ")}")
  (doseq [[n _] arities]
    (print "  ([")
    (print (->> (range n) (map #(str "arg" %)) (str/join " ")))
    (println "]")
    (println "  nil)"))
  (println "  )"))

(defn- emit-ns-form [{:keys [ns-path]}]
  ;; - docstring
  ;; - :refer-clojure :exclude var names
  ;; - :requires
  (print "(ns" (str/join "." ns-path))
  (println ")"))

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
        (run! emit-var (vals vars))))))

(defn- emit-namespaces
  [out-dir namespaces]
  (doseq [ns namespaces]
    (emit-namespace out-dir ns)))

(defn emit-project
  [out-dir namespaces]
  ;; deps.edn file
  ;; src directory
  ;; src/deps.cljs file w/ :npm-deps (?)
  ;; package.json file (?)
  (emit-namespaces (path/join out-dir "src") (vals namespaces)))
