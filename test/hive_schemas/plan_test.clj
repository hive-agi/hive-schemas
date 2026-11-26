(ns hive-schemas.plan-test
  "plan/validate and plan/explain share one helper, `against-plan`. Its
   trifecta: happy (a built plan), edge (a boundary plan), error (non-plans)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.plan :as plan]
            [hive-schemas.test :as hst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private against-plan #'hive-schemas.plan/against-plan)

(deftest against-plan-happy
  (testing "the helper hands the Plan key and the value to the check it is given"
    (is (= [:hive.schemas/plan 42] (against-plan vector 42))))
  (testing "a built plan validates and has no explanation"
    (let [p (plan/plan 'p 'a/b {:in :int :out :int :rel (fn [_ _] true)})]
      (is (true? (plan/validate p)))
      (is (nil? (plan/explain p))))))

(deftest against-plan-edge
  (testing "a plan with every facet and the :compiled provenance"
    (let [p (plan/plan 'all #'clojure.core/inc {:in :int :out :int :rel (fn [_ _] true)
                                   :idempotent? true :contract true :golden-path "g.edn"
                                   :strict-in true :prove {} :model-check {}
                                   :provenance :compiled})]
      (is (= plan/facet-order (:plan/facets p)))
      (is (true? (plan/validate p)))
      (is (nil? (plan/explain p)))))
  (testing "validate and explain never disagree"
    (doseq [v [{} nil 1 (plan/plan 'q 'a/b {})]]
      (is (= (plan/validate v) (nil? (plan/explain v))) (pr-str v)))))

(deftest against-plan-error
  (testing "non-plans fail validate and carry an explanation"
    (doseq [v [nil {} "plan" {:plan/name 'x}
               (assoc (plan/plan 'p 'a/b {}) :plan/facets [])
               (assoc (plan/plan 'p 'a/b {}) :plan/provenance :guessed)
               (assoc (plan/plan 'p 'a/b {}) :extra 1)]]
      (is (false? (plan/validate v)) (pr-str v))
      (is (some? (plan/explain v)) (pr-str v)))))

(defn validate-rel
  "validate answers exactly whether explain finds nothing to report."
  [p out]
  (= out (nil? (plan/explain p))))

(hst/deftrifecta-from-schema validate-facets plan/validate
  {:in        [:any {:gen/elements [nil {} 1 "x" {:plan/name 'x}
                                    (plan/plan 'p 'a/b {})
                                    (plan/plan 'q 'a/b {:in :int :contract true})]}]
   :out       :boolean
   :rel       hive-schemas.plan-test/validate-rel
   :mutation  false
   :num-tests 60})
