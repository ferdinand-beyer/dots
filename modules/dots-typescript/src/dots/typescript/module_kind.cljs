(ns dots.typescript.module-kind
  (:require ["typescript" :as typescript]))

(def none (.-None typescript/ModuleKind))

(def common-js (.-CommonJS typescript/ModuleKind))

(def amd (.-AMD typescript/ModuleKind))

(def umd (.-UMD typescript/ModuleKind))

(def system (.-System typescript/ModuleKind))

(def es-2015 (.-ES2015 typescript/ModuleKind))

(def es-2020 (.-ES2020 typescript/ModuleKind))

(def es-2022 (.-ES2022 typescript/ModuleKind))

(def es-next (.-ESNext typescript/ModuleKind))

(def node-16 (.-Node16 typescript/ModuleKind))

(def node-next (.-NodeNext typescript/ModuleKind))
