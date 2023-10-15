(ns dots.typescript.object-flags
  (:require ["typescript" :as typescript]))

(def class (.-Class typescript/ObjectFlags))

(def interface (.-Interface typescript/ObjectFlags))

(def reference (.-Reference typescript/ObjectFlags))

(def tuple (.-Tuple typescript/ObjectFlags))

(def anonymous (.-Anonymous typescript/ObjectFlags))

(def mapped (.-Mapped typescript/ObjectFlags))

(def instantiated (.-Instantiated typescript/ObjectFlags))

(def object-literal (.-ObjectLiteral typescript/ObjectFlags))

(def evolving-array (.-EvolvingArray typescript/ObjectFlags))

(def object-literal-pattern-with-computed-properties (.-ObjectLiteralPatternWithComputedProperties typescript/ObjectFlags))

(def reverse-mapped (.-ReverseMapped typescript/ObjectFlags))

(def jsx-attributes (.-JsxAttributes typescript/ObjectFlags))

(def js-literal (.-JSLiteral typescript/ObjectFlags))

(def fresh-literal (.-FreshLiteral typescript/ObjectFlags))

(def array-literal (.-ArrayLiteral typescript/ObjectFlags))

(def class-or-interface (.-ClassOrInterface typescript/ObjectFlags))

(def contains-spread (.-ContainsSpread typescript/ObjectFlags))

(def object-rest-type (.-ObjectRestType typescript/ObjectFlags))

(def instantiation-expression-type (.-InstantiationExpressionType typescript/ObjectFlags))
