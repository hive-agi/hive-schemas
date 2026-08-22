(ns hive-schemas.strength
  "How much a schema CONSTRAINS.

   Measured against a fixed value ladder, so scores are deterministic and
   comparable across schemas and runs.

   Levers:
     universe          the value ladder strength is measured against
     rejection-rate    ?s -> fraction of the universe the schema rejects
     schema-strength   ?s -> {:samples :accepted :rejected :rejection-rate
                              :degenerate?}
     degenerate?       ?s -> true when the schema rejects nothing
     input-vacuity     ?in -> nil | message
     type-degeneracy   ?s  -> nil | message
     registry-strength -> {schema-key strength} over the hive registry"
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

(m/=> type-degeneracy [:=> [:cat vocab/SchemaRef] vocab/Violation])

(m/=> registry-strength [:=> [:cat] [:map-of :any [:map]]])
