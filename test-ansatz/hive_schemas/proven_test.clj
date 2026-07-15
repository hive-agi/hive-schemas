(ns hive-schemas.proven-test
  "Rung-F proof (needs the ansatz kernel; runs under -M:ansatz-test only).

   Proves the TOP rung on the SAME subject shape rung E checks differentially
   (hive-schemas.verified-test's `vadd2`): one malli schema
   `[:=> [:cat :int :int] :int]` on an `a/defn`'d addition, then a behavioral
   property proven BY CONSTRUCTION in the CIC kernel via a/theorem.

   Three claims:
     1. left-identity  — vadd2 0 y = y, ∀ y (definitional; simp unfolds the fn).
     2. right-identity — vadd2 x 0 = x, ∀ x (induction on x; the base closes by
                         rfl, the step by the fn's equation lemmas + omega for the
                         1+n arithmetic). This is a genuine ∀ over ALL Nat, not a
                         sample.
     3. teeth          — vadd2 x 0 = succ x is FALSE (base case 0 = 1), so it has
                         no proof term: the kernel returns 'Proof incomplete' on the
                         unclosable goal. Rung F is COMPLETE where rung E's
                         differential sampling is sound-but-incomplete.

   Curried application `((vadd2 x) 0)` is the ansatz surface form. The proof
   reasons through vadd2's auto-generated equation lemmas — the same registered
   schema that yields the conformance / :contract / differential facets."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [malli.core :as m]
            [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kname]
            [hive-schemas.proven :as p]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; schema FIRST — the m/=> registry is namespace-keyed (like instrumentation);
;; the a/defn in the fixture elaborates this schema into the kernel term. Same
;; subject shape as hive-schemas.verified-test (rung E): one schema, both rungs.
(m/=> vadd2 [:=> [:cat :int :int] :int])

(defonce ^:private booted (delay (binding [a/*verbose* false] (a/load-init!))))

(defn- ensure-subject!
  "Boot the kernel stdlib, then elaborate `vadd2` from its m/=> schema into the
   live env (idempotent). load-init! re-inits the env per fixture, so this ns
   owns its subject regardless of test order (kaocha groups :once fixtures per
   ns)."
  []
  @booted
  (binding [a/*verbose* false]
    (when-not (env/lookup (a/env) (kname/from-string "vadd2"))
      (binding [*ns* (find-ns 'hive-schemas.proven-test)]
        (eval '(ansatz.core/defn vadd2 [x y]
                 (match x Nat Nat (zero y) (succ [k] (+ 1 (vadd2 k y)))))))))
  nil)

(use-fixtures :once (fn [t] (ensure-subject!) (t)))

;; --- claim 1: a definitional ∀ (left identity) ------------------------------
(p/deftrifecta-proven vadd2-left-identity
  :name    'hs-vadd2-left-id
  :params  '[y :- Nat]
  :prop    '(= Nat ((vadd2 (Nat.zero)) y) y)
  :tactics '[(simp [vadd2])])

;; --- claim 2: an inductive ∀ (right identity) — the star --------------------
;; Proven for ALL x : Nat by induction. all_goals + simp_all [vadd2] discharges
;; base + step through vadd2's equation lemmas; (try omega) closes the 1+n = succ n
;; arithmetic in the step. (User-authored, kernel-verified.)
(p/deftrifecta-proven vadd2-right-identity
  :name    'hs-vadd2-right-id
  :params  '[x :- Nat]
  :prop    '(= Nat ((vadd2 x) 0) x)
  :tactics '[(induction x) (all_goals (simp_all [vadd2])) (all_goals (try (omega)))])

;; --- claim 3: teeth — a FALSE property has NO proof (kernel soundness) -------
(deftest false-property-is-rejected
  (testing "the SAME tactic skeleton that proves vadd2-right-identity above,
            applied to the FALSE variant (vadd2 x 0 = succ x — false since
            vadd2 0 0 = 0, not 1), yields no proof term: the kernel returns
            'Proof incomplete' on the unclosable base goal. This completeness is
            exactly what rung E's differential sampling can never deliver."
    (let [msg (p/proof-failure 'hs-vadd2-false '[x :- Nat]
                               '(= Nat ((vadd2 x) 0) (Nat.succ x))
                               '[(induction x) (all_goals (simp_all [vadd2])) (all_goals (try (omega)))])]
      (is (some? msg) "the kernel must reject a proof of the false property")
      (is (re-find #"(?i)incomplete|not def|error" msg) (str "genuine kernel rejection: " msg)))))

;; --- honest-error surface: incomplete tactics -> a message, not a throw ------
(deftest incomplete-tactics-is-a-message
  (testing "proof-failure stays in the nil/message idiom when the tactics fail to
            close the goal (rfl alone cannot discharge the inductive goal)"
    (let [msg (p/proof-failure 'hs-vadd2-incomplete '[x :- Nat]
                               '(= Nat ((vadd2 x) 0) x)
                               '[(rfl)])]
      (is (some? msg) "an incomplete proof returns a message, not a throw"))))
