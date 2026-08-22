(ns hive-schemas.evolution
  "Registry snapshots and variance-correct compatibility checks.

   Compatibility is decided by SAMPLING, and every message says so.

   Levers:
     registry-snapshot  [ks] -> {schema-key {:form :json-schema :type}}
     compat-violation   old new opts -> nil | message
     breaking-changes   old-snapshot new-snapshot opts -> {schema-key message}
   One macro:
     deftest-schema-compat  name old new opts -> a compatibility facet"
  (:require [clojure.test]
            [hive-spi.schema.derive :as derive]
            [hive-spi.schema.gen :as sgen]
            [hive-spi.schema.registry :as reg]
            [hive-spi.schema.typed :as typed]
            [malli.core :as m]
            [hive-schemas.vocab :as vocab])
  #?(:cljs (:require-macros [hive-schemas.evolution])))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private default-samples 24)

(defn registry-snapshot
  "{schema-key {:form :json-schema :type}} for `ks` (default the whole hive
   registry), sorted.

   `:json-schema` is `::underivable` when the projection throws."
  ([] (registry-snapshot (keys (reg/registered))))
  ([ks]
   (into (sorted-map)
         (map (fn [k]
                [k {:form        (m/form (reg/schema k))
                    :json-schema (try (derive/input-schema k)
                                      (catch #?(:clj Exception :cljs :default) _
                                        ::underivable))
                    :type        (typed/schema->type k)}]))
         ks)))

(defn compat-violation
  "nil when `new` is a compatible evolution of `old` under `:variance`; else a
   message naming a value that shows it is not.

   `:input` (default) requires every value valid under `old` to stay valid under
   `new` — an input schema may only WIDEN. `:output` requires every value valid
   under `new` to have been valid under `old` — an output schema may only NARROW.

   SAMPLED over `:n` generated values."
  ([old new] (compat-violation old new {}))
  ([old new {:keys [variance seed n] :or {variance :input seed 0 n default-samples}}]
   (let [[source target] (case variance
                           :input  [old new]
                           :output [new old])
         valid?  (m/validator (reg/schema target))
         samples (vals (sgen/seeded-cases source seed n))]
     (when-let [bad (first (remove valid? samples))]
       (str "incompatible under " variance " variance (SAMPLED over " n
            " values, not proven): " (pr-str bad)
            " is valid under " (pr-str (m/form (reg/schema source)))
            " but not under " (pr-str (m/form (reg/schema target))))))))

(defn breaking-changes
  "{schema-key message} for every key of `old-snapshot` that `new-snapshot`
   breaks.

   A key absent from `new-snapshot` is breaking. A key whose form changed is
   checked with `compat-violation` under `:variance-by-key` (default
   `:default-variance`, itself `:input`)."
  ([old-snapshot new-snapshot] (breaking-changes old-snapshot new-snapshot {}))
  ([old-snapshot new-snapshot {:keys [variance-by-key default-variance]
                               :or   {variance-by-key {} default-variance :input}
                               :as   opts}]
   (into (sorted-map)
         (keep (fn [[k {old-form :form}]]
                 (if-let [entry (find new-snapshot k)]
                   (let [new-form (:form (val entry))]
                     (when (not= old-form new-form)
                       (when-let [msg (compat-violation
                                       old-form new-form
                                       (assoc opts :variance
                                              (get variance-by-key k default-variance)))]
                         [k msg])))
                   [k "schema removed from the registry"])))
         old-snapshot)))

(defmacro deftest-schema-compat
  "Emit `name` — a test asserting `new-snapshot` is a compatible evolution of
   `old-snapshot`.

   opts are `breaking-changes`'s."
  [name old-snapshot new-snapshot opts]
  (let [is-sym      (if (:ns &env) 'cljs.test/is 'clojure.test/is)
        deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)]
    `(~deftest-sym ~name
       (let [broken# (breaking-changes ~old-snapshot ~new-snapshot ~opts)]
         (~is-sym (empty? broken#)
                  (str "breaking schema changes: " (pr-str broken#)))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> registry-snapshot
      [:function
       [:=> [:cat] [:map-of :any [:map]]]
       [:=> [:cat [:seqable {:gen/elements [[]]} :any]] [:map-of :any [:map]]]])

(m/=> compat-violation
      [:function
       [:=> [:cat [vocab/SchemaRef {:gen/elements [:int :string :keyword [:map [:a :int]]]}]
                  [vocab/SchemaRef {:gen/elements [:int :string :keyword [:map [:a :int]]]}]]
        vocab/Violation]
       [:=> [:cat [vocab/SchemaRef {:gen/elements [:int :string :keyword [:map [:a :int]]]}]
                  [vocab/SchemaRef {:gen/elements [:int :string :keyword [:map [:a :int]]]}]
                  [:maybe vocab/Opts]]
        vocab/Violation]])

(m/=> breaking-changes
      [:function
       [:=> [:cat [:maybe [:map]] [:maybe [:map]]] [:map-of :any :string]]
       [:=> [:cat [:maybe [:map]] [:maybe [:map]] [:maybe vocab/Opts]] [:map-of :any :string]]])
