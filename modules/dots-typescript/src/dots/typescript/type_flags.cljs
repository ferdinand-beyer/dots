(ns dots.typescript.type-flags
  (:refer-clojure :exclude [boolean])
  (:require ["typescript" :as ts]))

(def any (.-Any ts/TypeFlags))
(def unknown (.-Unknown ts/TypeFlags))
(def string (.-String ts/TypeFlags))
(def number (.-Number ts/TypeFlags))
(def boolean (.-Boolean ts/TypeFlags))
(def enum (.-Enum ts/TypeFlags))
(def big-int (.-BigInt ts/TypeFlags))
(def string-literal (.-StringLiteral ts/TypeFlags))
(def number-literal (.-NumberLiteral ts/TypeFlags))
(def boolean-literal (.-BooleanLiteral ts/TypeFlags))
(def enum-literal (.-EnumLiteral ts/TypeFlags))
(def big-int-literal (.-BigIntLiteral ts/TypeFlags))
(def es-symbol (.-ESSymbol ts/TypeFlags))
(def unique-es-symbol (.-UniqueESSymbol ts/TypeFlags))
(def void (.-Void ts/TypeFlags))
(def undefined (.-Undefined ts/TypeFlags))
(def null (.-Null ts/TypeFlags))
(def never (.-Never ts/TypeFlags))
(def type-parameter (.-TypeParameter ts/TypeFlags))
(def object (.-Object ts/TypeFlags))
(def union (.-Union ts/TypeFlags))
(def intersection (.-Intersection ts/TypeFlags))
(def index (.-Index ts/TypeFlags))
(def indexed-access (.-IndexedAccess ts/TypeFlags))
(def conditional (.-Conditional ts/TypeFlags))
(def substitution (.-Substitution ts/TypeFlags))
(def non-primitive (.-NonPrimitive ts/TypeFlags))
(def template-literal (.-TemplateLiteral ts/TypeFlags))
(def string-mapping (.-StringMapping ts/TypeFlags))
(def literal (.-Literal ts/TypeFlags))
(def unit (.-Unit ts/TypeFlags))
(def string-or-number-literal (.-StringOrNumberLiteral ts/TypeFlags))
(def possibly-falsy (.-PossiblyFalsy ts/TypeFlags))
(def string-like (.-StringLike ts/TypeFlags))
(def number-like (.-NumberLike ts/TypeFlags))
(def big-int-like (.-BigIntLike ts/TypeFlags))
(def boolean-like (.-BooleanLike ts/TypeFlags))
(def enum-like (.-EnumLike ts/TypeFlags))
(def es-symbol-like (.-ESSymbolLike ts/TypeFlags))
(def void-like (.-VoidLike ts/TypeFlags))
(def union-or-intersection (.-UnionOrIntersection ts/TypeFlags))
(def structured-type (.-StructuredType ts/TypeFlags))
(def type-variable (.-TypeVariable ts/TypeFlags))
(def instantiable-non-primitive (.-InstantiableNonPrimitive ts/TypeFlags))
(def instantiable-primitive (.-InstantiablePrimitive ts/TypeFlags))
(def instantiable (.-Instantiable ts/TypeFlags))
(def structured-or-instantiable (.-StructuredOrInstantiable ts/TypeFlags))
(def narrowable (.-Narrowable ts/TypeFlags))