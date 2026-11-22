(ns hive-schemas.verified-test
  "Rung-E proof for the differential-verification facet (needs the ansatz Lean 4
   CIC kernel — runs ONLY under `clojure -M:ansatz-test`, never the pinned
   Maven build). Two claims:

     1. faithfulness  — an `a/defn` subject whose `m/=>` schema is registered
                        agrees with the kernel evaluator across generated inputs
                        (verified-divergence -> nil).
     2. teeth         — a well-typed but source-unfaithful runtime (an
                        elaboration bug) is CAUGHT here, though rung A (malli
                        conformance) is blind to it: its output still conforms.

   The single schema `[:=> [:cat :int :int] :int]` on `vadd2` is the shared
   source: malli reads it for conformance, the kernel term is elaborated from
   the SAME schema, and the differential lane pins runtime value against kernel
   whnf. Mirrors ansatz.malli-surface-test/test-differential-lane, but exercised
   through hive-schemas.verified (the nil/violation-message oracle idiom)."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [malli.core :as m]
            [hive-schemas.verified :as v]
            [hive-schemas.test-support :as ts]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; schema FIRST — the m/=> registry is namespace-keyed (like instrumentation);
;; `declare` gives the var a compile-time binding before the fixture's a/defn
;; sets its root AND installs the matching kernel term. The teeth test injects
;; its faulty runtime through verified-divergence's :runtime seam instead of
;; rebinding this var.
(declare vadd2)
(m/=> vadd2 [:=> [:cat :int :int] :int])

;; Boot the kernel once, then elaborate `vadd2` from its m/=> schema into the
;; live env (idempotent: skipped when already present).
(use-fixtures :once
  (ts/subject-fixture 'hive-schemas.verified-test
                      ["vadd2" '[(ansatz.core/defn vadd2 [x y]
                                   (match x Nat Nat (zero y) (succ [k] (+ 1 (vadd2 k y)))))]]))

;; --- claim 1: faithfulness (macro-synthesized deftest) -----------------------
;; vadd2's compiled runtime agrees with the kernel across 15 generated inputs.
(v/deftrifecta-verified vadd2-faithful hive-schemas.verified-test/vadd2 :runs 15)

;; --- claim 2: teeth — rung E catches what rung A cannot ----------------------
(deftest differential-catches-what-conformance-misses
  (testing "malli conformance (rung A) accepts a wrong-but-well-typed output;
            the differential lane (rung E) catches it against the kernel term"
    (let [buggy   (fn [x y] (+ x y 1))    ; well-typed: always an int, but +1 wrong
          int?-ok (m/validator :int)]
      ;; rung A is blind: the elaboration-bug output still conforms to :int
      (is (int?-ok (buggy 3 4))
          "conformance oracle accepts the wrong-but-well-typed value")
      ;; rung E is not: runtime (x+y+1) diverges from the kernel term (x+y).
      ;; The faulty runtime is INJECTED through the :runtime seam; vadd2's var
      ;; is never rebound.
      (let [msg (v/verified-divergence 'hive-schemas.verified-test 'vadd2 15
                                       {:runtime buggy})]
        (is (some? msg) "differential lane flags the divergence conformance missed")
        (is (re-find #"(?i)diverg" msg) (str "message names the divergence: " msg)))
      (is (nil? (v/verified-divergence 'hive-schemas.verified-test 'vadd2 15))
          "the real subject is untouched by the injected double"))))

;; --- honest-error surface: no m/=> schema -> a message, not a throw ----------
(deftest missing-schema-is-a-message
  (testing "verified-divergence stays in the nil/message idiom even when the
            subject carries no m/=> schema"
    (let [msg (v/verified-divergence 'hive-schemas.verified-test 'not-a-fn 5)]
      (is (some? msg) "missing-schema returns a message, does not throw"))))
