(ns dots.main
  (:require [dots.env :as env]
            [dots.extract :as extract]))

(defn- experiment []
  (let [env (env/of-modules ["vscode"])]
    (extract/extract env (get-in env [:module-symbols "vscode"]))))

(defn ^:export main []
  (println "Hello, Dots."))

(when goog/DEBUG
  (enable-console-print!))

(comment
  (def vscode (experiment))
  (run! prn (sort (keys (:exports vscode))))
  (get-in vscode [:exports "Command"])
  )
