(ns dots.env
  (:require [clojure.string :as str]
            [dots.typescript :as ts]
            [dots.typescript.import-declaration :as import-declaration]
            [dots.typescript.program :as program]
            [dots.typescript.source-file :as source-file]
            [dots.typescript.type-checker :as type-checker]))

(def ^:private proxy-file-name "dots$proxy.d.ts")

(defn- proxy-file-name? [file-name]
  (= file-name proxy-file-name))

(defn- proxy-source-text [module-names]
  (->> module-names
       (mapcat (fn [module-name]
                 (list "export * from \"" module-name "\";\n")))
       str/join))

(defn- proxy-compiler-host
  "Creates a CompilerHost that resolves our special proxy file."
  [compiler-opts module-names]
  (let [host        (ts/create-compiler-host compiler-opts true)
        source-text (proxy-source-text module-names)]
    (letfn [(file-exists [file-name]
              (or (proxy-file-name? file-name)
                  (.fileExists host file-name)))
            (get-source-file [file-name target-or-opts on-error create?]
              (if (proxy-file-name? file-name)
                (ts/create-source-file proxy-file-name source-text target-or-opts true)
                (.getSourceFile host file-name target-or-opts on-error create?)))]
      (.assign js/Object #js {} host #js {:fileExists    file-exists
                                          :getSourceFile get-source-file}))))

(defn- create-program
  ([module-names]
   (create-program module-names nil))
  ([module-names compiler-opts]
   (let [compiler-opts (or compiler-opts #js {})
         host (proxy-compiler-host compiler-opts module-names)]
     (ts/create-program #js [proxy-file-name] compiler-opts host))))

(defn- module-symbols [program]
  (let [checker     (program/get-type-checker program)
        source-file (program/get-source-file program proxy-file-name)]
    (->> (source-file/statements source-file)
         (map import-declaration/module-specifier)
         (map #(type-checker/get-symbol-at-location checker %)))))

(defn of-modules [module-names]
  (let [program      (create-program module-names)
        type-checker (program/get-type-checker program)]
    {:program      program
     :type-checker type-checker
     :module-symbols (zipmap module-names (module-symbols program))}))
