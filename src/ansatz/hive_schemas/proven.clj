(ns hive-schemas.proven
  "OPTIONAL full-CIC-proof facet — ladder rung F (the top rung).

   Loads only when org.replikativ/ansatz (the Lean 4 CIC kernel) is on the
   classpath. Where rung E (hive-schemas.verified) checks a compiled fn against
   the kernel DIFFERENTIALLY — sound but INCOMPLETE, only the sampled inputs —
   rung F proves a behavioral property BY CONSTRUCTION: an `a/theorem` whose
   proof term the kernel checks inhabits the stated Prop for ALL inputs. A false
   Prop has no proof term (kernel soundness), so `deftrifecta-proven` cannot pass
   on a false theorem — the completeness rung E's sampling can never give.

   The Prop is stated over an `a/defn`'d, malli-schema'd function: its
   auto-generated equation lemmas (f.eq_N, one per match leaf) drive the
   induction/simp proof, so the SAME registered schema that yields the
   conformance / :contract / differential facets is the source the proof reasons
   about. This is the single-source lever climbing its last rung.

   Single lever:
     proof-failure  thm-name params prop tactics -> nil | message
   One macro:
     deftrifecta-proven  name :params p :prop q :tactics tac [:name n]
                         -> a deftest asserting the kernel accepts the proof"
  (:require [clojure.test]
            [ansatz.core :as a]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn proof-failure
  "nil when the ansatz kernel ACCEPTS a proof of `prop` (under the typed
   `params`) via `tactics` — i.e. theorem `thm-name` installs + kernel-verifies;
   else a message carrying the kernel's rejection (incomplete proof, a
   non-definitionally-equal rfl, an unknown constant, ...). Wraps
   a/install-theorem! (idempotent, re-raises on failure) in the nil/violation-
   message idiom shared across hive-schemas (output-oracle, contract-violation,
   verified-divergence): a rejection becomes a value, not a throw, so callers
   stay in the same shape. `params`/`prop`/`tactics` are the quoted DATA forms
   the ansatz surface consumes, e.g. '[n :- Nat], '(= Nat (f n 0) n),
   '[(induction n) (rfl) (simp [f.eq_2 ih_n]) (omega)]. Kernel-only."
  [thm-name params prop tactics]
  (try
    (binding [a/*verbose* false]
      (a/install-theorem! thm-name params prop tactics))
    nil
    (catch Exception e
      (str "proof rejected — " (.getMessage e)))))

(defmacro deftrifecta-proven
  "Emit `name` — a clojure.test deftest asserting the ansatz kernel proves the
   behavioral property `:prop` (under `:params`) via `:tactics`, BY CONSTRUCTION
   (∀ inputs, not sampled). The test fails with the kernel's rejection message
   when the proof does not check. The subject function referenced in `:prop` must
   already be `a/defn`'d (typically in a :once fixture) so its equation lemmas
   exist when the test runs.

   opts:
     :params   typed param vector form   — '[n :- Nat]
     :prop     the Prop form             — '(= Nat (padd n 0) n)
     :tactics  the tactic vector form    — '[(induction n) (rfl) (simp [..]) (omega)]
     :name     kernel theorem name       — [default: `name`]"
  [name & {:keys [params prop tactics] tname :name}]
  `(clojure.test/deftest ~name
     (let [msg# (proof-failure '~(or tname name) ~params ~prop ~tactics)]
       (clojure.test/is (nil? msg#) (str msg#)))))
