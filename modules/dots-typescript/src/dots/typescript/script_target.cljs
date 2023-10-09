(ns dots.typescript.script-target
  (:require ["typescript" :as typescript]))

(def es-3 (.-ES3 typescript/ScriptTarget))

(def es-5 (.-ES5 typescript/ScriptTarget))

(def es-2015 (.-ES2015 typescript/ScriptTarget))

(def es-2016 (.-ES2016 typescript/ScriptTarget))

(def es-2017 (.-ES2017 typescript/ScriptTarget))

(def es-2018 (.-ES2018 typescript/ScriptTarget))

(def es-2019 (.-ES2019 typescript/ScriptTarget))

(def es-2020 (.-ES2020 typescript/ScriptTarget))

(def es-2021 (.-ES2021 typescript/ScriptTarget))

(def es-2022 (.-ES2022 typescript/ScriptTarget))

(def es-next (.-ESNext typescript/ScriptTarget))

(def json (.-JSON typescript/ScriptTarget))

(def latest (.-Latest typescript/ScriptTarget))
