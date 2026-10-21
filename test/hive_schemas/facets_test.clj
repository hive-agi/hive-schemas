(ns hive-schemas.facets-test
  "hive-test facets for hive-schemas' own contracted public API, synthesized by
   hive-schemas' own macros (dogfood).

   Each subject already carries an `m/=>` contract; these facets add the
   behavioural legs the contract cannot: a conformance property, a :rel that
   relates output to input, a malli-native contract facet (rung B), and the
   schema-derived mutation pair that proves the oracle has teeth."
  (:require [clojure.string :as str]
            [hive-schemas.coverage :as cov]
            [hive-schemas.emit :as emit]
            [hive-schemas.plan :as plan]
            [hive-schemas.strength :as strength]
            [hive-schemas.subject :as subject]
            [hive-schemas.test :as hst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; plan
;; =============================================================================

(def ^:private SelectorOpts
  "An options map over every facet selector, each present or absent, truthy or not."
  [:map
   [:rel {:optional true} [:maybe :symbol]]
   [:idempotent? {:optional true} :boolean]
   [:contract {:optional true} :boolean]
   [:mutation {:optional true} :boolean]
   [:golden-path {:optional true} [:maybe :string]]
   [:strict-in {:optional true} :boolean]
   [:prove {:optional true} [:maybe [:map]]]
   [:model-check {:optional true} [:maybe [:map]]]])

(defn opts->facets-rel
  "Conformance first, no duplicates, emission order, and each selector honoured
   by TRUTHINESS (`:mutation` defaulting to true)."
  [opts out]
  (and (= :conformance (first out))
       (apply distinct? out)
       (= out (filterv (set out) plan/facet-order))
       (= (boolean (:contract opts)) (contains? (set out) :contract))
       (= (boolean (:rel opts)) (contains? (set out) :relation))
       (= (boolean (:idempotent? opts)) (contains? (set out) :idempotent))
       (= (boolean (:golden-path opts)) (contains? (set out) :golden))
       (= (boolean (:strict-in opts)) (contains? (set out) :input-strength))
       (= (boolean (:prove opts)) (contains? (set out) :proof))
       (= (boolean (:model-check opts)) (contains? (set out) :model-check))
       (= (:mutation opts true)
          (contains? (set out) :mutations)
          (contains? (set out) :mutants-present))))

(hst/deftrifecta-from-schema opts->facets-facets plan/opts->facets
  {:in        SelectorOpts
   :out       [:vector {:min 1} plan/Facet]
   :rel       hive-schemas.facets-test/opts->facets-rel
   :contract  true
   :strict-in true
   :num-tests 60})

(defn facet-vars-rel
  "One var per facet, in order, each the plan name plus that facet's suffix."
  [p out]
  (and (= (count out) (count (:plan/facets p)))
       (every? true?
               (map (fn [v f]
                      (= (str v) (str (:plan/name p) (plan/facet-suffix f))))
                    out (:plan/facets p)))))

(hst/deftrifecta-from-schema facet-vars-facets plan/facet-vars
  {:in        [:map [:plan/name :symbol] [:plan/facets [:vector plan/Facet]]]
   :out       [:vector :symbol]
   :rel       hive-schemas.facets-test/facet-vars-rel
   :contract  true
   :num-tests 60})

(def ^:private PlanOpts
  "Options a plan carries: schemas, an optional symbolic :rel, macro knobs."
  [:map
   [:in :keyword]
   [:out :keyword]
   [:rel {:optional true} :qualified-symbol]
   [:contract {:optional true} :boolean]
   [:num-tests {:optional true} [:int {:min 1 :max 500}]]])

(defn plan-rel
  "The plan round-trips through its options and records :declared provenance."
  [[plan-name subj opts] p]
  (and (= (symbol (name plan-name)) (:plan/name p))
       (= subj (:plan/subject p))
       (= :declared (:plan/provenance p))
       (= opts (plan/plan->opts p))
       (= (plan/opts->facets opts) (:plan/facets p))))

(hst/deftrifecta-from-schema plan-facets plan/plan
  {:in        [:cat :symbol :qualified-symbol PlanOpts]
   :out       :hive.schemas/plan
   :rel       hive-schemas.facets-test/plan-rel
   :contract  true
   :strict-in true
   :num-tests 60})

(hst/deftrifecta-predicate plan-validate-facets plan/validate
  {:schema    :hive.schemas/plan
   :num-tests 40})

;; =============================================================================
;; subject
;; =============================================================================

(hst/deftrifecta-from-schema subject-symbol-facets subject/subject-symbol
  {:in          :qualified-symbol
   :out         :qualified-symbol
   :rel         =
   :idempotent? true
   :contract    true
   :strict-in   true
   :num-tests   60})

(def ^:private ArityProbe
  "Schemas whose arity is known: two arglist forms, the rest single values."
  [:enum :int :string :keyword [:cat :int] [:catn [:a :int]] [:map [:a :int]]
   [:vector :int] [:maybe :int]])

(defn arglist-schema?-rel
  "An argument list is exactly a :cat or a :catn."
  [s out]
  (= out (and (vector? s) (contains? #{:cat :catn} (first s)))))

(hst/deftrifecta-from-schema arglist-schema?-facets subject/arglist-schema?
  {:in        ArityProbe
   :out       :boolean
   :rel       hive-schemas.facets-test/arglist-schema?-rel
   :contract  true
   :strict-in true
   :num-tests 40})

;; =============================================================================
;; strength
;; =============================================================================

(defn floor-rel
  "The floor is the LARGEST k >= 1 with 4·k·variants <= samples (or 1 when no
   such k exists)."
  [[n v] out]
  (let [per (* 4 (max 1 v))]
    (and (<= 1 out)
         (or (= 1 out) (<= (* per out) n))
         (> (* per (inc out)) n))))

(hst/deftrifecta-from-schema default-classification-floor-facets
  strength/default-classification-floor
  {:in        [:cat [:int {:min 0 :max 10000}] [:int {:min 0 :max 64}]]
   :out       [:int {:min 1}]
   :rel       hive-schemas.facets-test/floor-rel
   :contract  true
   :strict-in true
   :num-tests 100})

(def ^:private SchemaProbe
  "A spread of schemas from the vacuous to the narrow."
  [:enum :any :int :string :keyword :boolean :symbol [:map] [:vector :int]
   [:= 0] [:maybe :int] [:or :int :string]])

(defn rejection-rate-rel
  "The rate agrees with the strength report and with `degenerate?`."
  [s out]
  (and (= out (:rejection-rate (strength/schema-strength s)))
       (= (zero? out) (strength/degenerate? s))))

(hst/deftrifecta-from-schema rejection-rate-facets strength/rejection-rate
  {:in        SchemaProbe
   :out       [:double {:min 0.0 :max 1.0}]
   :rel       hive-schemas.facets-test/rejection-rate-rel
   :contract  true
   :strict-in true
   :num-tests 40})

(defn classification-domain-rel
  "A set is taken verbatim."
  [s out]
  (and (= (count s) (count out)) (= s (set out))))

(hst/deftrifecta-from-schema classification-domain-facets strength/classification-domain
  {:in        [:set :keyword]
   :out       [:vector :keyword]
   :rel       hive-schemas.facets-test/classification-domain-rel
   :contract  true
   :strict-in true
   :num-tests 60})

(def ^:private Variant [:enum :a :b :c :d])

(defn starvation-rel
  "nil exactly when every declared variant reaches the floor; otherwise the
   message names each starved variant."
  [[freqs domain floor] out]
  (let [starved (remove #(<= floor (get freqs % 0)) domain)]
    (if (seq starved)
      (and (string? out) (every? #(str/includes? out (pr-str %)) starved))
      (nil? out))))

(hst/deftrifecta-from-schema classification-starvation-facets
  strength/classification-starvation
  {:in        [:cat [:map-of Variant [:int {:min 0 :max 12}]]
                    [:set {:min 1} Variant]
                    [:int {:min 1 :max 10}]]
   :out       [:maybe :string]
   :rel       hive-schemas.facets-test/starvation-rel
   :contract  true
   :strict-in true
   :num-tests 100})

;; =============================================================================
;; emit
;; =============================================================================

(defn ns->path-rel
  "Every `-` becomes `_`, the only `.` left introduces the extension, and the
   length is the name plus the extension."
  [[ns-sym ext] out]
  (and (not (str/includes? out "-"))
       (str/ends-with? out (str "." ext))
       (= 1 (count (filter #{\.} out)))
       (= (count out) (+ (count (str ns-sym)) 1 (count ext)))))

(hst/deftrifecta-from-schema ns->path-facets emit/ns->path
  {:in        [:cat :symbol [:enum "clj" "cljc" "cljs"]]
   :out       [:and :string [:re #"\.cljc?s?$"]]
   :rel       hive-schemas.facets-test/ns->path-rel
   :contract  true
   :strict-in true
   :num-tests 60})

(defn unprintable-rel
  "Plain data has no unprintable leaves."
  [_ out]
  (empty? out))

(hst/deftrifecta-from-schema unprintable-facets emit/unprintable
  {:in        [:vector [:or :int :keyword :string :symbol :boolean [:vector :int]
                        [:map-of :keyword :int]]]
   :out       [:vector :any]
   :rel       hive-schemas.facets-test/unprintable-rel
   :contract  true
   :strict-in true
   :num-tests 60})

(def ^:private PrintablePlan
  "A conforming plan whose options are all readable data."
  [:map {:closed true}
   [:plan/name :symbol]
   [:plan/subject :qualified-symbol]
   [:plan/in :keyword]
   [:plan/out :keyword]
   [:plan/facets [:vector {:min 1} plan/Facet]]
   [:plan/provenance plan/Provenance]
   [:plan/opts [:map-of [:enum :num-tests :seed :n-cases] [:int {:min 0 :max 100}]]]])

(defn plan->form-rel
  "The form calls the synthesis macro on the plan's name, subject and options."
  [p out]
  (= out (list 'hive-schemas.test/deftriad-from-schema
               (:plan/name p) (:plan/subject p) (plan/plan->opts p))))

(hst/deftrifecta-from-schema plan->form-facets emit/plan->form
  {:in        PrintablePlan
   :out       [:cat [:= 'hive-schemas.test/deftriad-from-schema] :symbol
               :qualified-symbol [:map [:in :keyword] [:out :keyword]]]
   :rel       hive-schemas.facets-test/plan->form-rel
   :contract  true
   :strict-in true
   :num-tests 40})

;; =============================================================================
;; coverage
;; =============================================================================

(def ^:private Report
  "The fields of a coverage report the gate reads."
  [:map
   [:paths [:vector :string]]
   [:universe [:set :symbol]]
   [:unreadable [:vector [:map [:file :string] [:error :string]]]]
   [:unloadable [:vector [:map [:ns :symbol] [:error :string]]]]
   [:missing [:set :symbol]]
   [:exempt [:map-of :symbol [:maybe :string]]]
   [:stale-exemptions [:set :symbol]]])

(defn coverage-failures-rel
  "One message per gate that does not hold, and none when every gate holds."
  [{:keys [universe unreadable unloadable missing exempt stale-exemptions]} out]
  (let [blank? (some (fn [[_ v]] (or (not (string? v)) (str/blank? v))) exempt)
        failed (count (filter true? [(empty? universe)
                                     (boolean (seq unreadable))
                                     (boolean (seq unloadable))
                                     (boolean (seq missing))
                                     (boolean blank?)
                                     (boolean (seq stale-exemptions))]))]
    (= failed (count out))))

(hst/deftrifecta-from-schema coverage-failures-facets cov/coverage-failures
  {:in        Report
   :out       [:sequential :string]
   :rel       hive-schemas.facets-test/coverage-failures-rel
   :contract  true
   :strict-in true
   :num-tests 60})
