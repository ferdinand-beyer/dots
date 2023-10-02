(ns inspect-flags
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]
            [dots.util.flags :as flags]))

(defn- parse-enum-line [line]
  (when-let [[_ name val] (re-matches #"\s*(\w+)\s*=\s*(\d+)\s*,?\s*" line)]
    [name val]))

(defn- parse-source [source]
  (->> source str/split-lines (keep parse-enum-line)))

(defn parse-type [source]
  (->> (parse-source source)
       (into {} (map (juxt (comp csk/->kebab-case-keyword first)
                           (comp parse-long second))))
       (flags/flags-type)))

(defn gen-vars! [flag-type source]
  (doseq [[name _val] (parse-source source)]
    (println (str "(def "
                  (csk/->kebab-case-string name)
                  " (.-" name " ts/" flag-type ")"
                  ")"))))

(defn- gen-map! [source]
  (doseq [[name val] (parse-source source)]
    (println " " (csk/->kebab-case-keyword name) val)))

(def symbol-flags
  (parse-type
   "    None = 0,
        FunctionScopedVariable = 1,
        BlockScopedVariable = 2,
        Property = 4,
        EnumMember = 8,
        Function = 16,
        Class = 32,
        Interface = 64,
        ConstEnum = 128,
        RegularEnum = 256,
        ValueModule = 512,
        NamespaceModule = 1024,
        TypeLiteral = 2048,
        ObjectLiteral = 4096,
        Method = 8192,
        Constructor = 16384,
        GetAccessor = 32768,
        SetAccessor = 65536,
        Signature = 131072,
        TypeParameter = 262144,
        TypeAlias = 524288,
        ExportValue = 1048576,
        Alias = 2097152,
        Prototype = 4194304,
        ExportStar = 8388608,
        Optional = 16777216,
        Transient = 33554432,
        Assignment = 67108864,
        ModuleExports = 134217728,
        Enum = 384,
        Variable = 3,
        Value = 111551,
        Type = 788968,
        Namespace = 1920,
        Module = 1536,
        Accessor = 98304,
        FunctionScopedVariableExcludes = 111550,
        BlockScopedVariableExcludes = 111551,
        ParameterExcludes = 111551,
        PropertyExcludes = 0,
        EnumMemberExcludes = 900095,
        FunctionExcludes = 110991,
        ClassExcludes = 899503,
        InterfaceExcludes = 788872,
        RegularEnumExcludes = 899327,
        ConstEnumExcludes = 899967,
        ValueModuleExcludes = 110735,
        NamespaceModuleExcludes = 0,
        MethodExcludes = 103359,
        GetAccessorExcludes = 46015,
        SetAccessorExcludes = 78783,
        TypeParameterExcludes = 526824,
        TypeAliasExcludes = 788968,
        AliasExcludes = 2097152,
        ModuleMember = 2623475,
        ExportHasLocal = 944,
        BlockScoped = 418,
        PropertyOrAccessor = 98308,
        ClassMember = 106500,"))

(def type-flags
  (parse-type
   "    Any = 1,
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
        Narrowable = 536624127,"))

(def object-flags
  (parse-type
   "    Class = 1,
        Interface = 2,
        Reference = 4,
        Tuple = 8,
        Anonymous = 16,
        Mapped = 32,
        Instantiated = 64,
        ObjectLiteral = 128,
        EvolvingArray = 256,
        ObjectLiteralPatternWithComputedProperties = 512,
        ReverseMapped = 1024,
        JsxAttributes = 2048,
        JSLiteral = 4096,
        FreshLiteral = 8192,
        ArrayLiteral = 16384,
        ClassOrInterface = 3,
        ContainsSpread = 2097152,
        ObjectRestType = 4194304,
        InstantiationExpressionType = 8388608,"))

(comment
  (flags/flags-seq type-flags 4)
  (flags/flags-seq type-flags 524288)
  (flags/flags-seq object-flags 524293)

  ;;
  )
