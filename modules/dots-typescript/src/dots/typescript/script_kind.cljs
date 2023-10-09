(ns dots.typescript.script-kind
  (:require ["typescript" :as typescript]))

(def unknown (.-Unknown typescript/ScriptKind))

(def js (.-JS typescript/ScriptKind))

(def jsx (.-JSX typescript/ScriptKind))

(def ts (.-TS typescript/ScriptKind))

(def tsx (.-TSX typescript/ScriptKind))

(def external (.-External typescript/ScriptKind))

(def json (.-JSON typescript/ScriptKind))

(def deferred
  "Used on extensions that doesn't define the ScriptKind but the content defines it.
   Deferred extensions are going to be included in all project contexts."
  (.-Deferred typescript/ScriptKind))
