(ns dots.main
  (:require [dots.adapt :as adapt]
            [dots.emit :as emit]
            [dots.env :as env]
            [dots.extract :as extract]))

(defn- extract-module [module-name]
  (let [env (env/of-modules [module-name])]
    (extract/extract-module env (get-in env [:module-symbols module-name]))))

(defn ^:export main []
  (println "Hello, Dots."))

(when goog/DEBUG
  (enable-console-print!))

(comment
  (def vscode (extract-module "vscode"))

  (run! prn (sort (keys (:exports vscode))))
  (get-in vscode [:exports "Event"])
  (get-in vscode [:exports "authentication"
                  :exports "getSession"])

  (get-in vscode [:exports "TextEditorSelectionChangeKind"])

  (def namespaces (adapt/adapt vscode))
  (get namespaces 'vscode)
  (get namespaces 'vscode.text-editor-selection-change-kind)

  (emit/emit-project nil nil)

  ;;
  )
