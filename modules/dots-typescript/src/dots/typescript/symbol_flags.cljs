(ns dots.typescript.symbol-flags
  (:refer-clojure :exclude [type namespace transient])
  (:require ["typescript" :as ts]))

(def none (.-None ts/SymbolFlags))
(def function-scoped-variable (.-FunctionScopedVariable ts/SymbolFlags))
(def block-scoped-variable (.-BlockScopedVariable ts/SymbolFlags))
(def property (.-Property ts/SymbolFlags))
(def enum-member (.-EnumMember ts/SymbolFlags))
(def function (.-Function ts/SymbolFlags))
(def class (.-Class ts/SymbolFlags))
(def interface (.-Interface ts/SymbolFlags))
(def const-enum (.-ConstEnum ts/SymbolFlags))
(def regular-enum (.-RegularEnum ts/SymbolFlags))
(def value-module (.-ValueModule ts/SymbolFlags))
(def namespace-module (.-NamespaceModule ts/SymbolFlags))
(def type-literal (.-TypeLiteral ts/SymbolFlags))
(def object-literal (.-ObjectLiteral ts/SymbolFlags))
(def method (.-Method ts/SymbolFlags))
(def constructor (.-Constructor ts/SymbolFlags))
(def get-accessor (.-GetAccessor ts/SymbolFlags))
(def set-accessor (.-SetAccessor ts/SymbolFlags))
(def signature (.-Signature ts/SymbolFlags))
(def type-parameter (.-TypeParameter ts/SymbolFlags))
(def type-alias (.-TypeAlias ts/SymbolFlags))
(def export-value (.-ExportValue ts/SymbolFlags))
(def alias (.-Alias ts/SymbolFlags))
(def prototype (.-Prototype ts/SymbolFlags))
(def export-star (.-ExportStar ts/SymbolFlags))
(def optional (.-Optional ts/SymbolFlags))
(def transient (.-Transient ts/SymbolFlags))
(def assignment (.-Assignment ts/SymbolFlags))
(def module-exports (.-ModuleExports ts/SymbolFlags))
(def enum (.-Enum ts/SymbolFlags))
(def variable (.-Variable ts/SymbolFlags))
(def value (.-Value ts/SymbolFlags))
(def type (.-Type ts/SymbolFlags))
(def namespace (.-Namespace ts/SymbolFlags))
(def module (.-Module ts/SymbolFlags))
(def accessor (.-Accessor ts/SymbolFlags))
(def function-scoped-variable-excludes (.-FunctionScopedVariableExcludes ts/SymbolFlags))
(def block-scoped-variable-excludes (.-BlockScopedVariableExcludes ts/SymbolFlags))
(def parameter-excludes (.-ParameterExcludes ts/SymbolFlags))
(def property-excludes (.-PropertyExcludes ts/SymbolFlags))
(def enum-member-excludes (.-EnumMemberExcludes ts/SymbolFlags))
(def function-excludes (.-FunctionExcludes ts/SymbolFlags))
(def class-excludes (.-ClassExcludes ts/SymbolFlags))
(def interface-excludes (.-InterfaceExcludes ts/SymbolFlags))
(def regular-enum-excludes (.-RegularEnumExcludes ts/SymbolFlags))
(def const-enum-excludes (.-ConstEnumExcludes ts/SymbolFlags))
(def value-module-excludes (.-ValueModuleExcludes ts/SymbolFlags))
(def namespace-module-excludes (.-NamespaceModuleExcludes ts/SymbolFlags))
(def method-excludes (.-MethodExcludes ts/SymbolFlags))
(def get-accessor-excludes (.-GetAccessorExcludes ts/SymbolFlags))
(def set-accessor-excludes (.-SetAccessorExcludes ts/SymbolFlags))
(def type-parameter-excludes (.-TypeParameterExcludes ts/SymbolFlags))
(def type-alias-excludes (.-TypeAliasExcludes ts/SymbolFlags))
(def alias-excludes (.-AliasExcludes ts/SymbolFlags))
(def module-member (.-ModuleMember ts/SymbolFlags))
(def export-has-local (.-ExportHasLocal ts/SymbolFlags))
(def block-scoped (.-BlockScoped ts/SymbolFlags))
(def property-or-accessor (.-PropertyOrAccessor ts/SymbolFlags))
(def class-member (.-ClassMember ts/SymbolFlags))

(comment
  (def flags
    {:None 0
     :FunctionScopedVariable 1
     :BlockScopedVariable 2
     :Property 4
     :EnumMember 8
     :Function 16
     :Class 32
     :Interface 64
     :ConstEnum 128
     :RegularEnum 256
     :ValueModule 512
     :NamespaceModule 1024
     :TypeLiteral 2048
     :ObjectLiteral 4096
     :Method 8192
     :Constructor 16384
     :GetAccessor 32768
     :SetAccessor 65536
     :Signature 131072
     :TypeParameter 262144
     :TypeAlias 524288
     :ExportValue 1048576
     :Alias 2097152
     :Prototype 4194304
     :ExportStar 8388608
     :Optional 16777216
     :Transient 33554432
     :Assignment 67108864
     :ModuleExports 134217728
     :Enum 384
     :Variable 3
     :Value 111551
     :Type 788968
     :Namespace 1920
     :Module 1536
     :Accessor 98304
     :FunctionScopedVariableExcludes 111550
     :BlockScopedVariableExcludes 111551
     :ParameterExcludes 111551
     :PropertyExcludes 0
     :EnumMemberExcludes 900095
     :FunctionExcludes 110991
     :ClassExcludes 899503
     :InterfaceExcludes 788872
     :RegularEnumExcludes 899327
     :ConstEnumExcludes 899967
     :ValueModuleExcludes 110735
     :NamespaceModuleExcludes 0
     :MethodExcludes 103359
     :GetAccessorExcludes 46015
     :SetAccessorExcludes 78783
     :TypeParameterExcludes 526824
     :TypeAliasExcludes 788968
     :AliasExcludes 2097152
     :ModuleMember 2623475
     :ExportHasLocal 944
     :BlockScoped 418
     :PropertyOrAccessor 98308
     :ClassMember 106500})

  (def rflags
    (into {} (map (juxt val key)) flags))

  (defn bits [field]
    (loop [bits nil
           bit 0x1]
      (if (<= bit field)
        (if (zero? (bit-and field bit))
          (recur bits (bit-shift-left bit 1))
          (recur (conj bits bit) (bit-shift-left bit 1)))
        bits)))

  (defn bits [field]
    (->> (iterate #(bit-shift-left % 1) 1)
         (take-while #(<= % field))
         (map #(bit-and field %))
         (remove zero?)))

  (bits 98308)

  (def kwflags (update-vals flags #(into #{} (map rflags) (bits %))))
  kwflags

  )
