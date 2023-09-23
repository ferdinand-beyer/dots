(ns flags
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

(defn- parse-enum-line [line]
  (when-let [[_ name val] (re-matches #"\s*(\w+)\s*=\s*(\d+)\s*,?\s*" line)]
    [name val]))

(defn- parse-source [source]
  (->> source str/split-lines (keep parse-enum-line)))

(defn gen-vars! [flag-type source]
  (doseq [[name _val] (parse-source source)]
    (println (str "(def "
                  (csk/->kebab-case-string name)
                  " (.-" name " ts/" flag-type ")"
                  ")"))))

(defn- gen-map! [source]
  (doseq [[name val] (parse-source source)]
    (println " " (csk/->kebab-case-keyword name) val)))

(comment
  (def source "
    Any = 1,
    Unknown = 2,
    String = 4,
    Number = 8,
    Boolean = 16,
    Enum = 32,
    BigInt = 64,
    StringLiteral = 128,
    NumberLiteral = 256,
    BooleanLiteral = 512,
    EnumLiteral = 1024,
    BigIntLiteral = 2048,
    ESSymbol = 4096,
    UniqueESSymbol = 8192,
    Void = 16384,
    Undefined = 32768,
    Null = 65536,
    Never = 131072,
    TypeParameter = 262144,
    Object = 524288,
    Union = 1048576,
    Intersection = 2097152,
    Index = 4194304,
    IndexedAccess = 8388608,
    Conditional = 16777216,
    Substitution = 33554432,
    NonPrimitive = 67108864,
    TemplateLiteral = 134217728,
    StringMapping = 268435456,
    Literal = 2944,
    Unit = 109440,
    StringOrNumberLiteral = 384,
    PossiblyFalsy = 117724,
    StringLike = 402653316,
    NumberLike = 296,
    BigIntLike = 2112,
    BooleanLike = 528,
    EnumLike = 1056,
    ESSymbolLike = 12288,
    VoidLike = 49152,
    UnionOrIntersection = 3145728,
    StructuredType = 3670016,
    TypeVariable = 8650752,
    InstantiableNonPrimitive = 58982400,
    InstantiablePrimitive = 406847488,
    Instantiable = 465829888,
    StructuredOrInstantiable = 469499904,
    Narrowable = 536624127,
   ")

  (gen-map! source)

  (def flags {:any   1
              :unknown   2
              :string   4
              :number   8
              :boolean   16
              :enum   32
              :big-int   64
              :string-literal   128
              :number-literal   256
              :boolean-literal   512
              :enum-literal   1024
              :big-int-literal   2048
              :es-symbol   4096
              :unique-es-symbol   8192
              :void   16384
              :undefined   32768
              :null   65536
              :never   131072
              :type-parameter   262144
              :object   524288
              :union   1048576
              :intersection   2097152
              :index   4194304
              :indexed-access   8388608
              :conditional   16777216
              :substitution   33554432
              :non-primitive   67108864
              :template-literal   134217728
              :string-mapping   268435456
              :literal   2944
              :unit   109440
              :string-or-number-literal   384
              :possibly-falsy   117724
              :string-like   402653316
              :number-like   296
              :big-int-like   2112
              :boolean-like   528
              :enum-like   1056
              :es-symbol-like   12288
              :void-like   49152
              :union-or-intersection   3145728
              :structured-type   3670016
              :type-variable   8650752
              :instantiable-non-primitive   58982400
              :instantiable-primitive   406847488
              :instantiable   465829888
              :structured-or-instantiable   469499904
              :narrowable   536624127})
  )

