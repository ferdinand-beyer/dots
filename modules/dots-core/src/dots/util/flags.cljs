(ns dots.util.flags)

(defn flag-test [flags flag]
  (not (zero? (bit-and flags flag))))

(defn bits [field]
  (->> (iterate #(bit-shift-left % 1) 1)
       (take-while #(<= % field))
       (map #(bit-and field %))
       (remove zero?)))

(defn flags-type [m]
  (let [r (into {} (map (juxt val key)) m)]
    (with-meta m {::r r})))

(defn- reverse-map [ft]
  (::r (meta ft)))

(defn rflag [ft flag]
  ((reverse-map ft) flag flag))

(defn flag [ft flag]
  (ft flag flag))

(comment
  (def ft (flags-type {:foo 1 :bar 2}))
  (flag ft :foo)
  (flag ft :bar)
  (flag ft 1)
  (rflag ft 1)
  )

(defn flags-seq [ft flags]
  (map #(rflag ft %) (bits flags)))

(comment
  (flags-seq (flags-type {:spam 1 :eggs 2}) 7)
  )

(defn flags [ft x]
  (cond
    (nil? x) 0
    (int? x) x
    (coll? x) (->> x (map #(flag ft %)) (reduce bit-or 0))
    :else (flag ft x)))

(comment
  (def ft (flags-type {:spam 1 :eggs 2 :tomato 4}))
  (flags ft [])
  (flags ft [:spam :tomato])
  (flags ft :tomato)
  )

(deftype Flag [name val]
  Object
  (toString [this]
    (pr-str* this))
  (equiv [this other]
    (-equiv this other))

  IEquiv
  (-equiv [_ other]
    (and (instance? Flag other)
         (= name (.name ^Flag other))
         (= val (.val ^Flag other))))

  IHash
  (-hash [_] (hash flags))

  IFn
  (-invoke [this flags]
    (when (flag-test flags val)
      this))
  (-invoke [this flags not-found]
    (if (flag-test flags val)
      this
      not-found))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#Flag[")
    (-pr-writer name writer opts)
    (-write writer " ")
    (-write writer (str val))
    (-write writer "]")))

(comment
  (def spam (Flag. 'spam 1))
  spam
  (spam 4)
  )

(deftype FlagSetIter [iter]
  Object
  (hasNext [_]
    #_(.hasNext iter))
  (next [_]
    #_(if ^boolean (.hasNext iter)
        (.-key (.next iter))
        (throw (js/Error. "No such element"))))
  (remove [_] (js/Error. "Unsupported operation")))

(deftype FlagSet [meta ft flags]
  Object
  (toString [this]
    (pr-str* this))
  (equiv [this other]
    (-equiv this other))

  ICloneable
  (-clone [_] (FlagSet. meta ft flags))

  IIterable
  (-iterator [_]
    (FlagSetIter. (-iterator (flags-seq ft flags))))

  IWithMeta
  (-with-meta [coll new-meta]
    (if (identical? new-meta meta)
      coll
      (FlagSet. new-meta ft flags)))

  IMeta
  (-meta [_] meta)

  ICollection
  (-conj [_ v]
    (FlagSet. meta ft (bit-or flags (flag ft v))))

  IEmptyableCollection
  (-empty [_] (FlagSet. meta ft 0))

  IEquiv
  (-equiv [coll other]
    (and
     (set? other)
     (== (count coll) (count other))
     ^boolean
     (try
       (reduce
        #(or (contains? other %2) (reduced false))
        true (flags-seq ft flags))
       (catch js/Error _ex
         false))))

  IHash
  (-hash [_] (hash flags))

  ISeqable
  (-seq [_] (flags-seq ft flags))

  ICounted
  (-count [_] (bit-count flags))

  ILookup
  (-lookup [coll v]
    (-lookup coll v nil))
  (-lookup [_ v not-found]
    (if (zero? (bit-and flags (flag ft v)))
      not-found
      v))

  ISet
  (-disjoin [_ v]
    (FlagSet. meta ft (bit-and-not flags (flag ft v))))

  IFn
  (-invoke [coll k]
    (-lookup coll k))
  (-invoke [coll k not-found]
    (-lookup coll k not-found))

  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-write writer "#FlagSet[")
    (-write writer (str flags))
    (-write writer " {")
    (when-let [s (seq (flags-seq ft flags))]
      (pr-seq-writer s writer opts))
    (-write writer "}]")))

(defn flag-set
  ([ft]
   (FlagSet. nil ft 0))
  ([ft init]
   (FlagSet. nil ft (flags ft init))))

(comment
  (def t (flags-type
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
           :ClassMember 106500}))

  (def expanded
    (update-vals t #(into #{} (map (reverse-map t)) (bits %))))
  (:EnumMember expanded)

  (seq (flags-seq t 106500))
  (seq (flag-set t :ExportHasLocal))
  (seq (flag-set t :ModuleMember))

  (str (flag-set t :ClassMember))

  #{1 2 3}

  (= (flag-set t 106500) (flag-set t :ClassMember))
  (= (flag-set t 106500)
     #{:Property :Method :GetAccessor :SetAccessor})

  (contains? (flag-set t 106500) :ClassMember)
  (:ExportHasLocal (flag-set t 106500))

  (str (flag-set t))

  (-> (flag-set t)
      (conj :BlockScoped)
      :Class)

  (flag-set t :ClassMember)

  ((flag-set t :ClassMember) :Method)
  ((flag-set t :ClassMember) :x)

  (contains? (flag-set t :ClassMember) :Property)

  ;;
  )
