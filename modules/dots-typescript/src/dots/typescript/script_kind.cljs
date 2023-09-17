(ns dots.typescript.script-kind)

(def unknown 0)
(def js 1)
(def jsx 2)
(def ts 3)
(def tsx 4)
(def external 5)
(def json 6)
;; Used on extensions that doesn't define the ScriptKind but the content defines it.
;; Deferred extensions are going to be included in all project contexts.
(def deferred 7)
