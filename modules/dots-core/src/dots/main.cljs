(ns dots.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [dots.adapt :as adapt]
            [dots.emit :as emit]
            [dots.extract :as extract]))

(def cli-opts
  (cond-> [["-h" "--help" "Show help"]
           ["-n" "--namespace NS" "Target namespace"]
           ["-o" "--output-dir DIR" "Output directory"
            :default "target/dots"]]
    goog/DEBUG (conj ["-r" "--repl" "REPL mode: don't exit"])))

(defn- exit [code]
  (.exit js/process code))

(defn- usage [summary]
  (println "Usage: dots [options] module")
  (println)
  (println "Options:")
  (println summary)
  (exit 0))

(defn cli-error [& errors]
  (run! println errors)
  (exit 1))

(defn- run
  [module-name {:keys [exit?]
                :or {exit? true}
                :as opts}]
  (let [module     (extract/extract module-name opts)
        namespaces (adapt/adapt module opts)]
    (emit/emit-project namespaces opts))
  (when exit?
    (exit 0)))

(defn ^:export main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-opts)]
    (cond
      (:help options) (usage summary)
      (:repl options) (println "REPL mode")
      errors (apply cli-error errors)
      (empty? arguments) (cli-error "Missing argument: \"module\"")
      (next arguments) (cli-error "Too many arguments")
      :else (run (first arguments) options))))

(when goog/DEBUG
  (enable-console-print!))

(comment
  (run "vscode" {:output-dir "target/dots-vscode", :exit? false})
  (run "typescript" {:output-dir "target/dots-typescript", :exit? false})
  (run "path" {:output-dir "target/dots-path", :exit? false})
  (run "fs" {:output-dir "target/dots-fs", :exit? false})

  (set! extract/*debug?* true)

  (def vscode (extract/extract "vscode" {}))
  (def ts (extract/extract "typescript" {}))

  (def path (extract/extract "path" {}))

  (run! prn (sort (keys (:exports vscode))))
  (get-in vscode [:exports "Event"])
  (get-in vscode [:exports "Command"])
  (get-in vscode [:exports "authentication" :exports])
  (get-in vscode [:exports "authentication"
                  :exports "getSession"])
  (get-in vscode [:exports "DataTransfer"])
  (get-in vscode [:exports "Uri" :exports "joinPath"])

  (get-in ts [:exports "TypeChecker"])

  (get-in vscode [:exports "TextEditorSelectionChangeKind"])

  (def namespaces (adapt/adapt vscode {}))
  (get namespaces "vscode")
  (get namespaces "vscode.text-editor")
  (get namespaces "vscode.text-editor-selection-change-kind")
  (get namespaces "vscode.authentication")

  (emit/emit-project namespaces {:output-dir "dist/dots-vscode"})

  ;;
  )
