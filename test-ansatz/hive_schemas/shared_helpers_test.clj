(ns hive-schemas.shared-helpers-test
  "Trifectas (happy, edge, error) for the two helpers the kernel suites share
   (needs the ansatz kernel; runs under -M:ansatz-test only):

     hive-schemas.proven/kernel-failure    the one failure constructor behind
                                           proof-failure and csimp-failure
     hive-schemas.test-support             boot!, kernel-has?, ensure-subject!,
                                           subject-fixture"
  (:require [clojure.test :refer [deftest testing is]]
            [ansatz.core :as a]
            [hive-schemas.proven :as p]
            [hive-schemas.test-support :as ts]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private kernel-failure #'hive-schemas.proven/kernel-failure)

;; =============================================================================
;; kernel-failure
;; =============================================================================

(deftest kernel-failure-happy
  (testing "an install that returns is nil, whatever it returns"
    (is (nil? (kernel-failure "proof" (fn [& _] :installed) 'n '[] '(= Nat 0 0) '[(rfl)])))
    (is (nil? (kernel-failure "csimp" (fn [& _] nil) 'n '[] 'p '[]))))
  (testing "the install receives the four kernel arguments in order"
    (let [seen (atom nil)]
      (kernel-failure "proof" (fn [& args] (reset! seen (vec args)))
                      'thm '[n :- Nat] '(= Nat n n) '[(rfl)])
      (is (= ['thm '[n :- Nat] '(= Nat n n) '[(rfl)]] @seen)))))

(deftest kernel-failure-edge
  (testing "the install runs with the kernel quiet, even when the caller is verbose"
    (let [seen (atom :unset)]
      (binding [a/*verbose* true]
        (kernel-failure "proof" (fn [& _] (reset! seen a/*verbose*)) 'n '[] 'p '[]))
      (is (false? @seen))))
  (testing "an exception with no message still becomes a message"
    (is (= "csimp rejected — "
           (kernel-failure "csimp" (fn [& _] (throw (RuntimeException.))) 'n '[] 'p '[])))))

(deftest kernel-failure-error
  (testing "a rejection is a value naming what was rejected and why"
    (is (= "proof rejected — Proof incomplete"
           (kernel-failure "proof" (fn [& _] (throw (ex-info "Proof incomplete" {})))
                           'n '[] 'p '[])))
    (is (= "csimp rejected — not defeq"
           (kernel-failure "csimp" (fn [& _] (throw (IllegalStateException. "not defeq")))
                           'n '[] 'p '[]))))
  (testing "the public levers keep their own prefixes on a real kernel rejection"
    (ts/boot! 'hive-schemas.shared-helpers-test)
    (is (re-find #"^proof rejected — "
                 (p/proof-failure 'hs-shared-bad '[] '(= Nat 0 1) '[(rfl)])))
    (is (re-find #"^csimp rejected — "
                 (p/csimp-failure 'hs-shared-bad-csimp '[]
                                  '(= (forall [_ :- Nat] Nat) Nat.succ Nat.pred) '[(rfl)])))))

;; =============================================================================
;; test-support
;; =============================================================================

(def ^:private this-ns 'hive-schemas.shared-helpers-test)

(defn- subject
  "The two forms that make an int -> int kernel subject: its m/=> schema, then
   the a/defn that elaborates from it. Order is load-bearing."
  [fname params body]
  [(list 'malli.core/=> fname [:=> [:cat :int] :int])
   (list 'ansatz.core/defn fname params body)])

(deftest ensure-subject-happy
  (testing "the forms are evaluated in the named ns and land in the kernel"
    (ts/ensure-subject! this-ns "hsSharedId" (subject 'hsSharedId '[n] 'n))
    (is (ts/kernel-has? "hsSharedId"))
    (is (= 7 ((resolve 'hive-schemas.shared-helpers-test/hsSharedId) 7)))))

(deftest ensure-subject-edge
  (testing "a subject already in the env is not elaborated again"
    (ts/ensure-subject! this-ns "hsSharedTwice" (subject 'hsSharedTwice '[n] 'n))
    (is (nil? (ts/ensure-subject! this-ns "hsSharedTwice" '[(throw (ex-info "re-evaluated" {}))]))))
  (testing "boot! is idempotent per namespace and answers nil"
    (is (nil? (ts/boot! this-ns)))
    (is (ts/kernel-has? "hsSharedTwice") "a second boot! did not re-init the env"))
  (testing "subject-fixture elaborates every pair, then runs the tests"
    (let [ran (atom false)]
      ((ts/subject-fixture this-ns
                           ["hsSharedA" (subject 'hsSharedA '[n] 'n)]
                           ["hsSharedB" (subject 'hsSharedB '[n] '(+ n 0))])
       #(reset! ran true))
      (is (true? @ran))
      (is (ts/kernel-has? "hsSharedA"))
      (is (ts/kernel-has? "hsSharedB")))))

(deftest ensure-subject-error
  (testing "an absent constant is reported absent"
    (ts/boot! this-ns)
    (is (false? (ts/kernel-has? "hsSharedNeverDefined"))))
  (testing "an elaboration error is thrown, not swallowed"
    (is (thrown? Exception
                 (ts/ensure-subject! this-ns "hsSharedBroken"
                                     (subject 'hsSharedBroken '[n] '(no-such-fn n))))))
  (testing "the fixture does not run the tests when its subject fails"
    (let [ran (atom false)]
      (is (thrown? Exception
                   ((ts/subject-fixture this-ns ["hsSharedBroken2" '[(throw (ex-info "boom" {}))]])
                    #(reset! ran true))))
      (is (false? @ran)))))
