(ns hive-schemas.raster
  "OPTIONAL rung-0 facet: a malli schema derived from a raster `deftm` signature.

   Loads only when org.replikativ/raster is on the classpath (the :raster alias).

   Levers:
     overloads        deftm-var -> [{:impl-var :name :params :tags :ret} ...]
     overload-schema  overload  -> {:in arglist-schema :out schema}
     infer-lengths    overload  -> {length-param #{array-param ...}} | nil
     approx= / approx-seq=      numeric :rel comparators"
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.test.check.generators :as gen]
            [raster.core :as rc]
            [raster.compiler.core.types :as rtypes]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def scalar-tag->schema
  "raster scalar tag -> malli schema."
  '{double :double, float :double, long :int, int :int, boolean :boolean, void :nil})

(def array-tag->element
  "raster array tag -> {:elem malli-schema :class array-class-name :make ctor}."
  {'doubles {:elem :double :class "[D" :make double-array}
   'floats  {:elem :double :class "[F" :make float-array}
   'longs   {:elem :int    :class "[J" :make long-array}
   'ints    {:elem :int    :class "[I" :make int-array}
   'bytes   {:elem :int    :class "[B" :make byte-array}})

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
   Throws for an unknown tag."
  [tag {:keys [magnitude] :or {magnitude 1.0e6}}]
  (case tag
    (double float) [:double {:min (- magnitude) :max magnitude}]
    (long int)     [:int {:min (- (long magnitude)) :max (long magnitude)}]
    boolean        :boolean
    void           :nil
    (throw (ex-info (str "no malli schema for raster tag `" tag "`")
                    {:tag tag
                     :known (vec (concat (keys scalar-tag->schema)
                                         (keys array-tag->element)))}))))

(defn array-gen
  "Generator producing a Java array of `tag` with exactly `len` elements."
  [tag len opts]
  (gen/fmap (:make (array-tag->element tag))
            (gen/vector (elem-gen tag opts) len)))

(defn array-schema
  "malli schema for an array parameter of `tag`: validates the Java array class,
   generates arrays of up to `:max-len` elements."
  [tag {:keys [max-len] :or {max-len 32} :as opts}]
  (let [klass (array-class tag)]
    [:fn {:error/message (str "must be a " tag " array")
          :gen/gen (gen/bind (gen/choose 0 max-len) #(array-gen tag % opts))}
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

   `opts` are `raster.core/resolve-deftm-var` opts; `{:dtype :double}` pins (and
   monomorphizes) exactly one overload. Without `:dtype`, enumerates the whole
   dispatch table. Throws when `v` is not a deftm var."
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
  [ov lengths {:keys [max-len] :or {max-len 32} :as opts}]
  (let [{:keys [params tags]} ov
        len-params (vec (keys lengths))
        governed   (into {} (for [[lp arrs] lengths, a arrs] [a lp]))]
    (gen/bind
     (apply gen/tuple (repeat (count len-params) (gen/choose 0 max-len)))
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

(m/=> array-tag? [:=> [:cat :any] :boolean])
(m/=> deftm? [:=> [:cat :any] :boolean])
(m/=> coupled? [:=> [:cat :map] :boolean])
(m/=> infer-lengths [:=> [:cat :map] [:maybe :map]])
(m/=> approx= [:function
               [:=> [:cat :double :double] :boolean]
               [:=> [:cat :double :double :double] :boolean]])
