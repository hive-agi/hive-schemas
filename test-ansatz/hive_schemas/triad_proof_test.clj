(ns hive-schemas.triad-proof-test
  "End-to-end TRIAD exemplar (needs the ansatz kernel; runs under -M:ansatz-test
   only). ONE deftriad-from-schema call fans a single subject out to the malli
   facets AND an ansatz kernel PROOF (rung F) — the malli+ansatz triad in one
   entry:

     addZeroX (n+0, an int->int a/defn'd fn) is the IDENTITY, checked THREE ways
     from ONE call:
       conformance : forall generated n, (addZeroX n) is an integer   (malli, sampled)
       relation    : forall generated n, (addZeroX n) = n             (malli, sampled)
       proof       : forall n:Nat, (addZeroX n) = n, BY CONSTRUCTION   (ansatz, rfl)

   The proof is the completeness the sampled facets cannot give: kernel soundness
   means a FALSE :prove prop has no proof term, so the emitted <name>-proof deftest
   fails loud (see hive-schemas.proven-test for the teeth). The a/defn'd subject
   returns kernel Nats (BigInt), so :out is [:fn integer?] and :in is bounded
   non-negative (Nat has no negatives); :mutation is off because a scalar output
   yields no schema-derived mutants."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [malli.core :as m]
            [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kname]
            [hive-schemas.test :as st]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; schema FIRST -- a/defn is malli-driven (the int -> Nat elaboration reads it).
(m/=> addZeroX [:=> [:cat :int] :int])
;; a/defn'd in the :once fixture; declared so the deftriad malli facets compile
;; before the kernel term (and its clojure fn) exist.
(declare addZeroX)

(defonce ^:private booted (delay (binding [a/*verbose* false] (a/load-init!))))

(defn- ensure-subject!
  "Boot the kernel stdlib, then elaborate addZeroX (n+0) from its m/=> schema into
   the live env + the clojure var (idempotent)."
  []
  @booted
  (binding [a/*verbose* false *ns* (find-ns 'hive-schemas.triad-proof-test)]
    (when-not (env/lookup (a/env) (kname/from-string "addZeroX"))
      (eval '(ansatz.core/defn addZeroX [n] (+ n 0)))))
  nil)

(use-fixtures :once (fn [t] (ensure-subject!) (t)))

;; ONE entry -> conformance + relation (malli, sampled) + proof (ansatz, forall).
(st/deftriad-from-schema triad-addzero addZeroX
  {:in       [:int {:min 0 :max 100000}]
   :out      [:fn integer?]
   :rel      (fn [n out] (= out n))
   :mutation false
   :prove    {:params '[n :- Nat] :prop '(= Nat (addZeroX n) n) :tactics '[(rfl)]}})

;; meta: the ONE call emitted all three facet vars, proof leg included.
(deftest triad-emits-malli-and-proof-facets
  (testing "deftriad-from-schema fanned out the malli facets AND the ansatz proof"
    (is (some? #'triad-addzero-conformance))
    (is (some? #'triad-addzero-relation))
    (is (some? #'triad-addzero-proof))))
