(ns dots.main
  (:require [clojure.tools.cli :refer [parse-opts]]
            [dots.adapt :as adapt]
            [dots.emit :as emit]
            [dots.env :as env]
            [dots.extract :as extract]))

(defn- extract-module [module-name]
  (let [env (env/of-modules [module-name])]
    (extract/extract-module env (get-in env [:module-symbols module-name]))))

(def cli-opts
  (cond-> [["-h" "--help" "Show help"]
           ["-o" "--output-dir" "Output directory"
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

(defn- run [module-name {:keys [exit?]
                         :or {exit? true}
                         :as opts}]
  (let [module     (extract-module module-name)
        namespaces (adapt/adapt module opts)]
    (emit/emit-project namespaces opts))
  (when exit?
    (exit 0)))

(defn ^:export main [& args]
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-opts)]
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
  (run "vscode" {:output-dir "dist/dots-vscode" :exit? false})

  (def vscode (extract-module "vscode"))

  (run! prn (sort (keys (:exports vscode))))
  (get-in vscode [:exports "Event"])
  (get-in vscode [:exports "TextDocument"])
  (get-in vscode [:exports "authentication" :exports])
  (get-in vscode [:exports "authentication"
                  :exports "getSession"])

  (get-in vscode [:exports "TextEditorSelectionChangeKind"])

  (def namespaces (adapt/adapt vscode {}))
  (get namespaces "vscode")
  (get namespaces "vscode.text-editor")
  (get namespaces "vscode.text-editor-selection-change-kind")
  (get namespaces "vscode.authentication")

  (emit/emit-project namespaces {:output-dir "dist/dots-vscode"})

  ;;
  )
