(ns hive-schemas.plan
  "Verification plans as VALUES.

   A plan names a subject, its input/output schemas and the facets to synthesize
   from them. The `hive-schemas.test` macros and `hive-schemas.emit` are
   projections of one.

   Registered schemas:
     :hive.schemas/facet       one facet id
     :hive.schemas/provenance  :declared | :inferred | :compiled
     :hive.schemas/plan        the Plan value object

   Levers:
     facet-order    facet ids, in emission order
     facet-suffix   facet id -> the suffix its emitted test var carries
     opts->facets   an options map -> the facets it selects
     plan           name subject opts -> Plan
     plan->opts     Plan -> the options map the macros consume
     facet-vars     Plan -> the test var symbols it emits
     subject-symbol ns/fn | #'ns/fn | var -> the bare qualified symbol
     validate/explain  a Plan against :hive.schemas/plan"
  (:require [hive-schemas.subject :as subject]
            [hive-spi.schema.registry :as reg]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; The facet table
;; =============================================================================

(def facet-order
  "Facet ids, in emission order. A closed set: `facet-suffix`, `opts->facets`,
   the emitting macro and the clj-kondo hook carry the same members."
  [:conformance :relation :idempotent :contract
   :mutants-present :mutations :golden :input-strength :proof :model-check])

(def facet-suffix
  "Facet id -> the suffix appended to a plan's name for that facet's test var."
  {:conformance     "-conformance"
   :relation        "-relation"
   :idempotent      "-idempotent"
   :contract        "-contract"
   :mutants-present "-mutants-present"
   :mutations       "-mutations"
   :golden          "-golden"
   :input-strength  "-input-strength"
   :proof           "-proof"
   :model-check     "-model-check"})

(defn opts->facets
  "Ordered facet ids the options map `opts` selects.

   Every selector reads the opt's TRUTHINESS, never its presence: `{:contract
   false}` selects no contract facet. `:mutation` defaults to true and selects
   the mutation pair."
  [{:keys [rel idempotent? contract mutation golden-path strict-in prove
           model-check]
    :or   {mutation true}}]
  (into []
        (remove nil?)
        [:conformance
         (when rel :relation)
         (when idempotent? :idempotent)
         (when contract :contract)
         (when mutation :mutants-present)
         (when mutation :mutations)
         (when golden-path :golden)
         (when strict-in :input-strength)
         (when prove :proof)
         (when model-check :model-check)]))

;; =============================================================================
;; Subject normalization
;; =============================================================================

(def subject-symbol
  "The bare qualified symbol behind a subject. See `hive-schemas.subject`."
  subject/subject-symbol)

;; =============================================================================
;; The Plan value object
;; =============================================================================

(def Facet
  "One facet id."
  (into [:enum] facet-order))

(def Provenance
  "How a plan's contract was obtained: written by a human (:declared), inferred
   by a tool (:inferred), or lowered by a compiler (:compiled)."
  [:enum :declared :inferred :compiled])

(def Plan
  "A verification plan: subject, schemas, selected facets, provenance, and the
   remaining macro options."
  [:map {:closed true}
   [:plan/name :symbol]
   [:plan/subject :qualified-symbol]
   [:plan/in :any]
   [:plan/out :any]
   [:plan/rel {:optional true} :any]
   [:plan/facets [:vector {:min 1} Facet]]
   [:plan/provenance Provenance]
   [:plan/opts [:map]]])

(reg/register-all! {:hive.schemas/facet      Facet
                    :hive.schemas/provenance Provenance
                    :hive.schemas/plan       Plan})

(def ^:private carried-by-plan
  "Option keys the Plan carries in its own fields rather than in :plan/opts."
  #{:in :out :rel :provenance})

(defn plan
  "Plan value for `subject` under `opts`.

   `plan-name` is the base name of the emitted test vars; `subject` is anything
   `subject-symbol` accepts. `:provenance` defaults to `:declared`."
  [plan-name subject opts]
  (cond-> {:plan/name        (symbol (name plan-name))
           :plan/subject     (subject-symbol subject)
           :plan/in          (:in opts)
           :plan/out         (:out opts)
           :plan/facets      (opts->facets opts)
           :plan/provenance  (:provenance opts :declared)
           :plan/opts        (apply dissoc opts carried-by-plan)}
    (:rel opts) (assoc :plan/rel (:rel opts))))

(defn plan->opts
  "The options map `hive-schemas.test`'s macros consume for `p`."
  [p]
  (cond-> (assoc (:plan/opts p) :in (:plan/in p) :out (:plan/out p))
    (contains? p :plan/rel) (assoc :rel (:plan/rel p))))

(defn facet-vars
  "The test var symbols `p` emits, in emission order."
  [p]
  (let [base (:plan/name p)]
    (mapv (fn [facet] (symbol (str base (facet-suffix facet)))) (:plan/facets p))))

(defn validate
  "True iff `p` conforms to :hive.schemas/plan."
  [p]
  (reg/validate :hive.schemas/plan p))

(defn explain
  "malli explanation for `p` against :hive.schemas/plan, or nil."
  [p]
  (reg/explain :hive.schemas/plan p))
