(ns hive-schemas.raster
  "OPTIONAL rung-0 facet: a malli schema derived from a raster `deftm` signature.

   Loads only when org.replikativ/raster is on the classpath (the :raster alias).

   Levers, forward:
     overloads         deftm-var -> [{:impl-var :name :params :tags :ret} ...]
     overload-schema   overload  -> {:in arglist-schema :out schema}
     kernel-schema     deftm-var + opts -> the ONE selected overload's {:in :out}
     infer-lengths     overload  -> {length-param #{array-param ...}} | nil
   and reverse:
     param-annotations    arglist schema -> [p :- Ann, ...]
     overload-annotations the same, straight from an overload
     return-annotation    an overload's `:- ` return annotation
   plus:
     approx= / approx-seq= / approx-rel   float comparators, and the :rel built
                                          from a pure reference
     element-types    the ONE table every projection above is derived from
     tc-extensions    the checker extensions `hive-schemas.typed-check` has to
                      :prepare before checking a raster-typed namespace"
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.test.check.generators :as gen]
            [raster.core :as rc]
            [raster.compiler.core.types :as rtypes]
            [hive-schemas.schema :as hss]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def tc-extensions
  "raster's Typed Clojure extensions. Pass as `hive-schemas.typed-check`'s
   `:prepare` when checking a namespace whose types depend on raster's value
   propagation or numeric promotion — they register by side effect at load time,
   so a check that runs before them reports the stock checker's verdict."
  'raster.compiler.core.tc-extensions)

(def element-types
  "One row per raster element type — the single source every projection reads.
     :tag         scalar tag as it appears in a mangled deftm name
     :schema      malli schema for a scalar of that tag
     :annot       deftm signature annotation for a scalar, when one is spelled
     :array-tag   array tag whose elements have this tag, when one exists
     :array-annot deftm signature annotation for that array
     :class       JVM class name of that array
     :make        that array's constructor
     :canonical?  true on the ONE row `:schema` maps back to"
  [{:tag 'double :schema :double :annot 'Double :canonical? true
    :array-tag 'doubles :array-annot '(Array double) :class "[D" :make double-array}
   {:tag 'float :schema :double :annot 'Float
    :array-tag 'floats :array-annot '(Array float) :class "[F" :make float-array}
   {:tag 'long :schema :int :annot 'Long :canonical? true
    :array-tag 'longs :array-annot '(Array long) :class "[J" :make long-array}
   {:tag 'int :schema :int
    :array-tag 'ints :array-annot '(Array int) :class "[I" :make int-array}
   {:tag 'byte :schema :int :annot 'Byte
    :array-tag 'bytes :array-annot '(Array byte) :class "[B" :make byte-array}
   {:tag 'boolean :schema :boolean :annot 'Boolean :canonical? true}
   {:tag 'void :schema :nil :annot 'Void :canonical? true}])

(def array-tag->element
  "raster array tag -> {:elem malli-schema :class array-class-name :make ctor}."
  (into {} (keep (fn [{:keys [array-tag schema class make]}]
                   (when array-tag [array-tag {:elem schema :class class :make make}])))
        element-types))

(def scalar-tag->schema
  "raster scalar tag -> malli schema."
  (into {} (map (juxt :tag :schema)) element-types))

(def tag->annotation
  "raster tag, scalar or array -> the annotation a deftm signature spells it with.
   `int` has no scalar annotation in raster and is absent as a scalar key."
  (into {} (mapcat (fn [{:keys [tag annot array-tag array-annot]}]
                     (cond-> []
                       annot (conj [tag annot])
                       (and array-tag array-annot) (conj [array-tag array-annot]))))
        element-types))

(def canonical-schema->tag
  "malli schema -> the ONE raster scalar tag it maps back to."
  (into {} (keep (fn [{:keys [canonical? schema tag]}] (when canonical? [schema tag])))
        element-types))

(def canonical-schema->array-tag
  "malli element schema -> the ONE raster array tag it maps back to."
  (into {} (keep (fn [{:keys [canonical? schema array-tag]}]
                   (when (and canonical? array-tag) [schema array-tag])))
        element-types))

(defn array-tag?
  "True when `tag` names a raster array type."
  [tag]
  (contains? array-tag->element tag))

(defn- array-class [tag]
  (Class/forName (:class (array-tag->element tag))))

(defn- elem-gen [tag {:keys [magnitude] :or {magnitude 1.0e6}}]
  (case (:elem (array-tag->element tag))
    :double (gen/double* {:min (- magnitude) :max magnitude :infinite? false :NaN? false})
    :int    (gen/choose (- (long magnitude)) (long magnitude))))

(defn scalar-schema
  "malli schema for a scalar parameter of `tag`. Bounded by `:magnitude`.
   Carries `:raster/tag` so the reverse direction reads the tag back instead of
   inferring it. Throws for an unknown tag."
  [tag {:keys [magnitude] :or {magnitude 1.0e6}}]
  (let [props {:raster/tag tag}]
    (case (or (get scalar-tag->schema tag)
              (throw (ex-info (str "no malli schema for raster tag `" tag "`")
                              {:tag tag
                               :known (vec (concat (keys scalar-tag->schema)
                                                   (keys array-tag->element)))})))
      :double  [:double (assoc props :min (- magnitude) :max magnitude)]
      :int     [:int (assoc props :min (- (long magnitude)) :max (long magnitude))]
      :boolean [:boolean props]
      :nil     [:nil props])))

(defn array-gen
  "Generator producing a Java array of `tag` with exactly `len` elements."
  [tag len opts]
  (gen/fmap (:make (array-tag->element tag))
            (gen/vector (elem-gen tag opts) len)))

(defn array-schema
  "malli schema for an array parameter of `tag`: validates the Java array class,
   generates arrays of `:min-len` to `:max-len` elements. Carries `:raster/tag`
   so the reverse direction reads the tag back instead of inferring it."
  [tag {:keys [min-len max-len] :or {min-len 0 max-len 32} :as opts}]
  (let [klass (array-class tag)]
    [:fn {:raster/tag tag
          :error/message (str "must be a " tag " array")
          :gen/gen (gen/bind (gen/choose min-len max-len) #(array-gen tag % opts))}
     #(instance? klass %)]))

(defn deftm?
  "True when `v` is a raster deftm var — dispatch (generic) var or backing impl var."
  [v]
  (let [mm (meta v)]
    (boolean (or (:raster.core/dispatch-table mm) (:raster.core/deftm mm)))))

(defn- impl-var->overload [iv]
  (let [mm (meta iv)]
    {:impl-var iv
     :name     (:name mm)
     :params   (vec (:raster.core/deftm-params mm))
     :tags     (vec (:raster.core/deftm-tags mm))
     :ret      (:raster.core/return-tag mm)}))

(defn overloads
  "Every concrete overload of deftm var `v`, as
   `[{:impl-var :name :params :tags :ret} ...]`.

   `opts` are `raster.core/resolve-deftm-var` opts; `{:dtype :double}` pins
   exactly one overload. Without `:dtype`, enumerates the dispatch table AS IT
   STANDS. Throws when `v` is not a deftm var.

   `:dtype` MONOMORPHIZES, and that REGISTERS the instantiation in the var's
   shared dispatch table — a later `:dtype`-less enumeration of the same var is
   wider. Enumerating a parametric `(All [T])` deftm is a fact about the running
   image, not about the definition."
  ([v] (overloads v nil))
  ([v opts]
   (let [mm (meta v)]
     (cond
       (:raster.core/deftm mm)
       [(impl-var->overload v)]

       (:raster.core/dispatch-table mm)
       (if (:dtype opts)
         [(impl-var->overload (rc/resolve-deftm-var v opts))]
         (let [ns-obj (:ns mm), fn-name (:name mm)]
           (->> @(:raster.core/dispatch-table mm)
                vals
                (apply concat)
                (keep #(ns-resolve ns-obj (rtypes/mangle fn-name (:tags %))))
                (mapv impl-var->overload))))

       :else
       (throw (ex-info (str "not a raster deftm var: " v) {:var v}))))))

(defn coupled?
  "True when `ov` has both an array parameter and an integer parameter."
  [ov]
  (boolean (and (some array-tag? (:tags ov))
                (some #{'long 'int} (:tags ov)))))

(defn infer-lengths
  "Heuristic `{length-param #{array-param ...}}` for `ov`, or nil.

   Fires only for exactly one integer parameter named `n` / `len` / `count` /
   `size` / `length` governing every array parameter; returns nil otherwise."
  [ov]
  (let [idx  (map vector (:params ov) (:tags ov))
        ints (filterv (comp #{'long 'int} second) idx)
        arrs (filterv (comp array-tag? second) idx)]
    (when (and (= 1 (count ints))
               (seq arrs)
               (contains? '#{n len count size length} (ffirst ints)))
      {(ffirst ints) (set (map first arrs))})))

(defn- coupled-generator
  "Generator for the whole argument vector: one length drawn per group in
   `lengths`, every array in that group built to exactly that length."
  [ov lengths {:keys [min-len max-len] :or {min-len 0 max-len 32} :as opts}]
  (let [{:keys [params tags]} ov
        len-params (vec (keys lengths))
        governed   (into {} (for [[lp arrs] lengths, a arrs] [a lp]))]
    (gen/bind
     (apply gen/tuple (repeat (count len-params) (gen/choose min-len max-len)))
     (fn [lens]
       (let [len-of (zipmap len-params lens)]
         (apply gen/tuple
                (map (fn [p t]
                       (cond
                         (contains? len-of p) (gen/return (get len-of p))
                         (array-tag? t)       (array-gen t (get len-of (governed p)) opts)
                         :else                (mg/generator (m/schema (scalar-schema t opts)))))
                     params tags)))))))

(defn arglist-schema
  "malli `:catn` arglist schema for one overload.

   opts:
     :lengths   {length-param #{array-param ...}} — REQUIRED when `coupled?`
     :min-len   smallest generated array (default 0)
     :max-len   largest generated array (default 32)
     :magnitude bound on generated numbers (default 1e6)

   Throws for a coupled overload with no `:lengths`."
  [ov {:keys [lengths] :as opts}]
  (when (and (coupled? ov) (not lengths))
    (throw (ex-info
            (str "overload `" (:name ov) "` couples an array parameter with an "
                 "integer parameter; pass :lengths {length-param #{array-param ...}}")
            {:overload (:name ov) :params (:params ov) :tags (:tags ov)
             :inferred (infer-lengths ov)})))
  (let [entries (mapv (fn [p t]
                        [(keyword (name p))
                         (if (array-tag? t) (array-schema t opts) (scalar-schema t opts))])
                      (:params ov) (:tags ov))]
    (m/schema (into (if lengths
                      [:catn {:gen/gen (coupled-generator ov lengths opts)}]
                      [:catn])
                    entries))))

(defn return-schema
  "malli schema for `ov`'s return type. A `:- Void` kernel returns `:nil`."
  [ov opts]
  (let [t (:ret ov)]
    (if (array-tag? t) (array-schema t opts) (scalar-schema t opts))))

(defn overload-schema
  "`{:in arglist-schema :out schema}` for one overload — the two keys
   `hive-schemas.test/deftrifecta-from-schema` requires."
  ([ov] (overload-schema ov nil))
  ([ov opts]
   {:in (arglist-schema ov opts)
    :out (return-schema ov opts)}))

(defn approx=
  "Relative-or-absolute float comparison for a numeric `:rel`.
   Relative below the crossover, absolute above it."
  ([a b] (approx= a b 1.0e-9))
  ([a b eps]
   (let [a (double a), b (double b)
         d (Math/abs (- a b))
         s (max (Math/abs a) (Math/abs b))]
     (or (<= d eps) (<= d (* eps s))))))

(defn approx-seq=
  "`approx=` elementwise over two seqable numeric collections (Java arrays
   included). Length mismatch is false, never a throw."
  ([a b] (approx-seq= a b 1.0e-9))
  ([a b eps]
   (let [a (seq a), b (seq b)]
     (and (= (count a) (count b))
          (every? true? (map #(approx= %1 %2 eps) a b))))))

(defn- base-type
  "malli type keyword of `schema`, properties stripped."
  [schema]
  (m/type (m/schema schema)))

(defn schema->tag
  "Canonical raster SCALAR tag for malli `schema`, or nil."
  [schema]
  (get canonical-schema->tag (base-type schema)))

(defn schema->array-tag
  "Canonical raster ARRAY tag for a malli sequential-of-scalar `schema`, or nil.
   The `:fn` schema `array-schema` emits carries no element type and yields nil."
  [schema]
  (let [s (m/schema schema)]
    (when (contains? #{:vector :sequential :set} (m/type s))
      (get canonical-schema->array-tag (base-type (first (m/children s)))))))

(defn schema->annotation
  "The raster `:- ` annotation for malli `schema`, or nil when no raster type
   corresponds.

   A schema this namespace built carries `:raster/tag` and inverts EXACTLY.
   A hand-written one is inverted by canonical inference, which is lossy:
   `float` shares `:double` with `double` and `int` shares `:int` with `long`,
   so the canonical row wins."
  [schema]
  (or (some-> (:raster/tag (m/properties (m/schema schema))) tag->annotation)
      (some-> (schema->tag schema) tag->annotation)
      (some-> (schema->array-tag schema) tag->annotation)))

(defn param-annotations
  "raster deftm parameter vector for a malli `:catn` arglist schema:
   `[:catn [:x :double] [:xs [:vector :double]]]` -> `[x :- Double, xs :- (Array double)]`.
   Throws for a non-`:catn` schema, and for an entry no raster type corresponds to."
  [arglist]
  (let [s (m/schema arglist)]
    (when-not (= :catn (m/type s))
      (throw (ex-info "param-annotations needs a :catn arglist schema"
                      {:type (m/type s) :schema (m/form s)})))
    (into []
          (mapcat (fn [[k _props child]]
                    (let [a (schema->annotation child)]
                      (when-not a
                        (throw (ex-info (str "no raster annotation for entry `" k "`")
                                        {:entry k :schema (m/form child)})))
                      [(symbol (name k)) :- a])))
          (m/children s))))

(defn overload-annotations
  "raster deftm parameter vector reconstructed from `ov` directly:
   `[dy :- Double, pred :- (Array double), target :- (Array double), n :- Long]`.
   The projection `param-annotations` is checked against. Throws for a tag no
   annotation spells."
  [ov]
  (vec (mapcat (fn [p t]
                 (let [a (get tag->annotation t)]
                   (when-not a
                     (throw (ex-info (str "no deftm annotation for raster tag `" t "`")
                                     {:tag t :overload (:name ov)})))
                   [p :- a]))
               (:params ov) (:tags ov))))

(defn return-annotation
  "raster `:- ` annotation for `ov`'s return type, or nil when none is spelled."
  [ov]
  (get tag->annotation (:ret ov)))

(defn kernel-schema
  "`{:in :out}` for the ONE overload of deftm var `v` that `opts` selects.

   `:lengths :infer` resolves through `infer-lengths` and throws when it
   declines. Throws unless `opts` selects exactly one overload."
  [v opts]
  (let [ovs (overloads v opts)]
    (when-not (= 1 (count ovs))
      (throw (ex-info (str "kernel-schema needs exactly one overload, got " (count ovs))
                      {:var v :tags (mapv :tags ovs)})))
    (let [ov   (first ovs)
          opts (if (= :infer (:lengths opts))
                 (assoc opts :lengths
                        (or (infer-lengths ov)
                            (throw (ex-info (str "infer-lengths declined overload `" (:name ov) "`")
                                            {:overload (:name ov) :params (:params ov)}))))
                 opts)]
      (overload-schema ov opts))))

(defn overload-keys
  "The registry keys `ov`'s schemas are registered under:
   `:<impl-ns>/<mangled-name>.in` and `.out`. The mangled name carries the
   overload's tags, so two instantiations of one deftm never collide."
  [ov]
  (let [ns-sym (ns-name (:ns (meta (:impl-var ov))))]
    [(keyword (str ns-sym) (str (:name ov) ".in"))
     (keyword (str ns-sym) (str (:name ov) ".out"))]))

(defn register-overload!
  "Register `ov`'s arglist and return schemas in the hive-spi schema registry
   under `overload-keys`, so a consumer reaches them by key instead of
   re-deriving them. Returns the two keys."
  [ov opts]
  (let [{:keys [in out]} (overload-schema ov opts)
        [kin kout]       (overload-keys ov)]
    (hss/register! kin in)
    (hss/register! kout out)
    [kin kout]))

(defn approx-rel
  "A `:rel` for `hive-schemas.test/deftrifecta-from-schema` built from a pure
   `reference`: `(fn [args out] ...)` asserting `out` matches
   `(apply reference args)` under `approx=` for a scalar and `approx-seq=` for
   a collection. opts: `:eps` (default 1e-9)."
  ([reference] (approx-rel reference nil))
  ([reference {:keys [eps] :or {eps 1.0e-9}}]
   (fn [args out]
     (let [expected (apply reference args)]
       (if (number? expected)
         (and (number? out) (approx= expected out eps))
         (approx-seq= expected out eps))))))

(m/=> array-tag? [:=> [:cat :any] :boolean])
(m/=> deftm? [:=> [:cat :any] :boolean])
(m/=> coupled? [:=> [:cat :map] :boolean])
(m/=> infer-lengths [:=> [:cat :map] [:maybe :map]])
(m/=> approx= [:function
               [:=> [:cat :double :double] :boolean]
               [:=> [:cat :double :double :double] :boolean]])
