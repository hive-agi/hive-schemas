(ns hive-schemas.foreign-lift-test
  "Rung F over a FOREIGN function: the forms a hive-shape language tier's
   `lift` answers, evaluated under the ansatz kernel and proven there.

   hive-shape's `hive-shape.lang.evidence/lift` answers two plain forms for
   a declaration inside the kernel fragment, the `m/=>` registration and the
   `a/defn`. Measured 2026-09-10, the five tiers (Zig, Go, Rust, Python, Elm)
   answer these SAME two forms for their `add` fixtures, modulo the name:

       (malli.core/=> add [:=> [:cat :int :int] :int])
       (ansatz.core/defn add [a b] (+ a b))

   They are evaluated here AS DATA, exactly as a consumer holding a
   LiftAnswer would, so this namespace needs no tier library and no
   hive-shape: what it proves is that the lift's OUTPUT is a kernel
   definition the ladder's top rung can discharge. `a/defn` with plain
   parameters consults the malli registry for its signature, which is the
   gradual path ansatz.malli documents and the reason the tier only ever
   states ONE malli schema.

   Three claims, the same shape as `proven-test`'s over `vadd2`:
     1. right identity, add x 0 = x, for ALL x by induction;
     2. left identity, add 0 y = y;
     3. teeth: add x 0 = succ x is FALSE and has no proof term."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kname]
            [hive-schemas.proven :as p]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def lifted-forms
  "What `lift` answered, verbatim, for every tier's `add`."
  '[(malli.core/=> add [:=> [:cat :int :int] :int])
    (ansatz.core/defn add [a b] (+ a b))])

(defonce ^:private booted (delay (binding [a/*verbose* false] (a/load-init!))))

(defn- ensure-subject!
  "Boot the kernel stdlib, then eval the lifted forms in THIS namespace, in
   order, so the m/=> registration is namespace-keyed where a/defn looks for
   it. Idempotent per kernel env."
  []
  @booted
  (binding [a/*verbose* false]
    (when-not (env/lookup (a/env) (kname/from-string "add"))
      (binding [*ns* (find-ns 'hive-schemas.foreign-lift-test)]
        (doseq [f lifted-forms] (eval f)))))
  nil)

(use-fixtures :once (fn [t] (ensure-subject!) (t)))

(deftest the-lifted-forms-elaborate
  (testing "the a/defn is a kernel constant after eval, and the compiled fn runs"
    (is (some? (env/lookup (a/env) (kname/from-string "add"))))
    (is (= 5 ((resolve 'hive-schemas.foreign-lift-test/add) 2 3)))))

(p/deftrifecta-proven foreign-add-right-identity
  :name    'hs-foreign-add-right-id
  :params  '[x :- Nat]
  :prop    '(= Nat ((add x) 0) x)
  :tactics '[(induction x) (all_goals (simp_all [add])) (all_goals (try (omega)))])

(p/deftrifecta-proven foreign-add-left-identity
  ;; `0 + y = y` is not definitional for Nat.add (recursion is on the second
  ;; argument), so it is proven by induction on y like the right identity.
  :name    'hs-foreign-add-left-id
  :params  '[y :- Nat]
  :prop    '(= Nat ((add 0) y) y)
  :tactics '[(induction y) (all_goals (simp_all [add])) (all_goals (try (omega)))])

(deftest a-false-property-of-the-lifted-function-is-rejected
  (let [msg (p/proof-failure 'hs-foreign-add-false '[x :- Nat]
                             '(= Nat ((add x) 0) (Nat.succ x))
                             '[(induction x) (all_goals (simp_all [add])) (all_goals (try (omega)))])]
    (is (some? msg) "the kernel must reject a proof of the false property")))
