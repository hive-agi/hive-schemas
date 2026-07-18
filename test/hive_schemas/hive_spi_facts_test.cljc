(ns hive-schemas.hive-spi-facts-test
  "Free hive-schemas.test coverage for the hive-spi.workflow.facts value-objects.
   Fact / FactSet are TUPLE / SET schemas (not maps), so the map-keyed mutation
   lever yields no mutants — the teeth come instead from a PINNED :out (the same
   registered schema) plus an identity :rel (edn->fact / edn->fact-set are
   reflexive over conformant facts). Generation + conformance are synthesized
   from the schemas hive-spi registered under :hive.workflow.facts/* — the
   single-source lever, driven here by registry KEY (no re-registration)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as tcgen]
            [hive-spi.workflow.facts :as facts]
            [hive-schemas.test :as hst]))

;; The schemas are registered at load of hive-spi.workflow.facts; reference them
;; by their stable registry keys (single source of truth).
(def ^:private Fact    :hive.workflow.facts/fact)
(def ^:private FactSet :hive.workflow.facts/fact-set)

;; Round-trip is reflexive over conformant facts: generate ~ Fact, output pinned
;; to Fact (op-shape), and the relation demands OUTPUT EQUALS INPUT (a strong
;; oracle standing in for the map-mutation facet these tuple/set schemas cannot
;; supply). :mutation false — required-entries is map-only, so no sound mutants.
(hst/deftrifecta-from-schema fact-roundtrip-tests facts/edn->fact
  {:in Fact :out Fact :num-tests 80 :mutation false
   :rel (fn [in out] (= in out))})

(hst/deftrifecta-from-schema factset-roundtrip-tests facts/edn->fact-set
  {:in FactSet :out FactSet :num-tests 50 :mutation false
   :rel (fn [in out] (= in out))})

;; Non-vacuity guard: the schema-derived oracle actually discriminates — it
;; accepts real facts / fact-sets and rejects structurally-corrupted values.
(deftest facts-oracle-has-teeth
  (let [fact-ok? (hst/output-oracle Fact)
        set-ok?  (hst/output-oracle FactSet)]
    (is (fact-ok? [:tests/green]))                    ; propositional fact
    (is (fact-ok? [:carto/scanned "hive-spi" 3]))     ; ground literal args
    (is (not (fact-ok? [:foo])))                      ; head not namespaced
    (is (not (fact-ok? ["tests-green"])))             ; head not a keyword
    (is (not (fact-ok? [:tests/green 'x])))           ; symbol arg = variable
    (is (set-ok? #{[:tests/green] [:work/committed]}))
    (is (not (set-ok? [[:tests/green]])))))           ; vector, not a set

;; The input generator is non-empty and yields canonical vector facts.
(deftest generator-yields-valid-facts
  (let [g (hst/input-gen Fact)]
    (is (every? facts/fact? (tcgen/sample g 20)))))
