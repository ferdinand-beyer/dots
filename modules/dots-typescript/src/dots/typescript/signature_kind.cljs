(ns dots.typescript.signature-kind
  (:require ["typescript" :as ts]))

(def call (.-Call ts/SignatureKind))
(def construct (.-Construct ts/SignatureKind))
