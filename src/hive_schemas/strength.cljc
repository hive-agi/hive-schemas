(ns hive-schemas.strength
  "How much a schema CONSTRAINS, and whether a property actually REACHES what
   it claims to cover.

   Two orthogonal axes of vacuity. Schema strength is measured against a fixed
   value ladder, so scores are deterministic and comparable across schemas and
   runs. Classification coverage is measured against an observed frequency map,
   because whether a branch is reached is a fact about the GENERATOR, not about
   the schema.

   Levers:
     universe          the value ladder strength is measured against
     rejection-rate    ?s -> fraction of the universe the schema rejects
     schema-strength   ?s -> {:samples :accepted :rejected :rejection-rate
                              :degenerate?}
     degenerate?       ?s -> true when the schema rejects nothing
     input-vacuity     ?in -> nil | message
     type-degeneracy   ?s  -> nil | message
     registry-strength -> {schema-key strength} over the hive registry

     classification-domain       ?domain -> [variant ...] (m/children, or a set
                                 taken verbatim)
     default-classification-floor n-samples n-variants -> int
     classification-starvation   freqs ?domain floor -> nil | message"
  (:require [hive-spi.schema.registry :as reg]
            [hive-spi.schema.typed :as typed]
            [malli.core :as m]
            [hive-schemas.vocab :as vocab]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def universe
  "Type-spanning values a schema's strength is measured against. Fixed and
   ordered; widening it changes every recorded score."
  [nil
   true
   false
   0
   -1
   42
   3.5
   ""
   "hive-schemas/x"
   :k
   :hive-schemas/k
   'sym
   'ns/sym
   []
   [1 2 3]
   ["a"]
   {}
   {:a 1}
   {"a" 1}
   #{}
   #{1 2}
   '(1 2)
   \c])

(defn rejection-rate
  "Fraction of `universe` that `?schema` REJECTS, in [0.0 1.0]. 0.0 means the
   schema accepts every value on the ladder."
  [?schema]
  (let [valid? (m/validator (reg/schema ?schema))
        n      (count universe)]
    (/ (double (count (remove valid? universe))) n)))

(defn schema-strength
  "How much `?schema` constrains, as data. `:degenerate?` is true when nothing on
   the ladder is rejected."
  [?schema]
  (let [valid?   (m/validator (reg/schema ?schema))
        accepted (filterv valid? universe)
        rejected (- (count universe) (count accepted))]
    {:samples       (count universe)
     :accepted      (count accepted)
     :rejected      rejected
     :rejection-rate (/ (double rejected) (count universe))
     :degenerate?   (zero? rejected)}))

(defn degenerate?
  "True when `?schema` rejects nothing on the universe ladder."
  [?schema]
  (:degenerate? (schema-strength ?schema)))

(defn input-vacuity
  "nil when `?in-schema` rejects something on the universe ladder; else a message
   naming the vacuity."
  [?in-schema]
  (let [{:keys [degenerate? rejection-rate]} (schema-strength ?in-schema)]
    (when degenerate?
      (str "vacuous :in — the schema accepts every value on the universe ladder "
           "(rejection-rate " rejection-rate "); the generated inputs constrain "
           "nothing. Tighten :in, or drop :strict-in."))))

(defn classification-domain
  "The variants `?domain` names, as a vector.

   A SET is taken verbatim. Anything else is read as a schema and its variants
   are its `m/children`, so adding a variant to the schema DEMANDS coverage
   instead of silently diluting the property."
  [?domain]
  (if (set? ?domain)
    (vec ?domain)
    (vec (m/children (reg/schema ?domain)))))

(defn default-classification-floor
  "Minimum times each of `n-variants` must be reached across `n-samples`: a
   quarter of an even split, never below 1."
  [n-samples n-variants]
  (max 1 (quot n-samples (* 4 (max 1 n-variants)))))

(defn classification-starvation
  "nil when every variant of `?domain` appears at least `floor` times in
   `freqs`; else a message naming the starved variants and the observed
   frequencies.

   `freqs` is variant -> count over a sample of classified outputs. The floor
   is a real floor, not `pos?` — one accidental collision is not coverage.

   This is the DISTRIBUTION axis of vacuity, orthogonal to `input-vacuity`:
   an `:in` can score full strength on the universe ladder and still never
   generate the input RELATIONSHIP a branch is gated on."
  [freqs ?domain floor]
  (let [variants (classification-domain ?domain)
        starved  (->> variants
                      (keep (fn [v] (let [c (get freqs v 0)]
                                      (when (< c floor) [v c]))))
                      (sort-by (comp str first))
                      vec)]
    (when (seq starved)
      (str "starved classification — " (count starved) " of " (count variants)
           " variant(s) reached fewer than " floor " time(s): " (pr-str starved)
           ". Observed " (pr-str (vec (sort-by (comp str first) freqs)))
           ". The generated inputs never exercise those branches, so the property"
           " is SILENT about them. Correlate the generator (:gen/schema +"
           " :gen/fmap on :in), or lower :classify-floor.")))) 

(defn type-degeneracy
  "nil when `?schema`'s Typed Clojure projection carries information; else a
   message. `hive-spi.schema.typed` maps `:fn`, unknown nodes and unresolvable
   refs to `typed.clojure/Any`."
  [?schema]
  (let [t (typed/schema->type ?schema)]
    (when (= 'typed.clojure/Any t)
      (str "degenerate type projection — " (pr-str (m/form (reg/schema ?schema)))
           " derives typed.clojure/Any, so the schema-as-type rung says nothing "
           "about this schema."))))

(defn registry-strength
  "{schema-key strength} for every schema currently in the hive registry."
  []
  (into (sorted-map)
        (map (fn [k] [k (schema-strength k)]))
        (keys (reg/registered))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> rejection-rate [:=> [:cat vocab/SchemaRef] :double])

(m/=> schema-strength
      [:=> [:cat vocab/SchemaRef]
       [:map [:samples :int] [:accepted :int] [:rejected :int]
             [:rejection-rate :double] [:degenerate? :boolean]]])

(m/=> degenerate? [:=> [:cat vocab/SchemaRef] :boolean])

(m/=> input-vacuity [:=> [:cat vocab/SchemaRef] vocab/Violation])

(m/=> classification-domain [:=> [:cat :any] [:vector :any]])

(m/=> default-classification-floor [:=> [:cat :int :int] :int])

(m/=> classification-starvation
      [:=> [:cat [:map-of :any :int] :any :int] vocab/Violation])

(m/=> type-degeneracy [:=> [:cat vocab/SchemaRef] vocab/Violation])

(m/=> registry-strength [:=> [:cat] [:map-of :any [:map]]])
