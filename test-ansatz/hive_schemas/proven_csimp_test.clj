(ns hive-schemas.proven-csimp-test
  "Rung-F -> REFACTOR bridge (needs the ansatz kernel; runs under -M:ansatz-test
   only). Proves a proof-carrying compiled refactor via ansatz csimp: the SAME
   registered malli schema that yields the conformance / :contract / differential
   / proof facets also licenses a compiler replacement `f -> g`, kernel-verified.

   Exemplar: `addZero` (n + 0, the naive form) and `ident` (n, the optimized
   form) are both `int -> int` malli-schema'd fns. `n + 0` is DEFINITIONALLY equal
   to `n` (Nat.add recurses on its second argument, so the `+ 0` base case
   iota-reduces), so `(= (forall [_ :- Nat] Nat) addZero ident)` closes by `(rfl)`
   -- forall n, not sampled. The proof registers `addZero -> ident` in the :csimp
   env extension: codegen then emits `ident` wherever `addZero` appears in
   COMPILED code (never in proofs), exactly like Lean's @[csimp]. The proof IS the
   licence.

   Teeth: `addZero = Nat.succ` is FALSE (addZero 0 = 0, succ 0 = 1), well-typed,
   so it has NO proof term -- the kernel rejects it. A non-equivalent refactor can
   never obtain a licence: kernel soundness is the safety of the swap."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [malli.core :as m]
            [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kname]
            [hive-schemas.proven :as p]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; schemas FIRST -- m/=> is namespace-keyed; the a/defn in the fixture elaborates
;; each schema's kernel term (int -> Nat). One schema per subject, the same
;; source the wider lever projects conformance / generators from.
(m/=> addZero [:=> [:cat :int] :int])
(m/=> ident   [:=> [:cat :int] :int])

(defonce ^:private booted (delay (binding [a/*verbose* false] (a/load-init!))))

(defn- ensure-subjects!
  "Boot the kernel stdlib (medium tier -- no Mathlib), then elaborate `addZero`
   (n+0) and `ident` (n) from their m/=> schemas into the live env (idempotent).
   load-init! re-inits the env per fixture, so this ns owns its subjects
   regardless of test order."
  []
  @booted
  (binding [a/*verbose* false
            *ns* (find-ns 'hive-schemas.proven-csimp-test)]
    (when-not (env/lookup (a/env) (kname/from-string "addZero"))
      (eval '(ansatz.core/defn addZero [n] (+ n 0))))
    (when-not (env/lookup (a/env) (kname/from-string "ident"))
      (eval '(ansatz.core/defn ident [n] n))))
  nil)

(use-fixtures :once (fn [t] (ensure-subjects!) (t)))

;; --- the licence: a proof-carrying compiled refactor ------------------------
;; addZero (n+0) == ident (n) forall n, by iota-reduction of Nat.add's base case.
;; The proof registers addZero -> ident as a verified compiler replacement.
(p/deftrifecta-csimp addzero-elim-csimp
  :name    'hs-addzero-elim
  :params  '[]
  :prop    '(= (forall [_ :- Nat] Nat) addZero ident)
  :tactics '[(rfl)])

;; --- teeth: a non-equivalent g gets NO licence (kernel soundness) -----------
(deftest non-equivalent-refactor-is-rejected
  (testing "addZero = Nat.succ is FALSE (addZero 0 = 0, not 1 = succ 0), so the
            kernel finds no proof term -- csimp-failure returns a rejection
            message, never a silent pass. The swap is only ever licensed by a
            genuine equivalence proof."
    (let [msg (p/csimp-failure 'hs-addzero-bad '[]
                               '(= (forall [_ :- Nat] Nat) addZero Nat.succ)
                               '[(rfl)])]
      (is (some? msg) "a non-equivalent refactor must be rejected")
      (is (re-find #"(?i)not definitionally|incomplete|error|reject" msg)
          (str "genuine kernel rejection: " msg)))))

;; --- honest-error surface: an unclosable proof is a message, not a throw -----
(deftest incomplete-proof-is-a-message
  (testing "csimp-failure stays in the nil/message idiom when the tactics fail to
            close a true goal (no tactics cannot discharge the fn-equality)"
    (let [msg (p/csimp-failure 'hs-addzero-incomplete '[]
                               '(= (forall [_ :- Nat] Nat) addZero ident)
                               '[])]
      (is (some? msg) "an incomplete csimp returns a message, not a throw"))))
