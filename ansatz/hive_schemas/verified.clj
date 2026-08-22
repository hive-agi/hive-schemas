(ns hive-schemas.verified
  "OPTIONAL differential-verification facet — ladder rung E.

   Loads only when org.replikativ/ansatz (the Lean 4 CIC kernel) is on the
   classpath. Turns an `a/defn`'d subject whose `m/=>` schema is registered into
   a clojure.test deftest that runs `ansatz.malli/check-verified!`: the COMPILED
   runtime is differentially checked against the kernel's whnf evaluation on
   schema-generated inputs. A divergence is an ELABORATION bug — a well-typed but
   source-unfaithful program the kernel's type-check alone cannot see.

   This is strictly stronger than the malli rungs (conformance / :contract):
   those pin the output SHAPE and a relation you wrote; the differential lane
   pins the output VALUE against an independent evaluator of the same source.

   Scope: Nat / Bool / (List Nat) arguments and results (ansatz differential v1).
   map / keyword ops are Opaque carriers in v1 — stay on the malli rungs for them.

   Single lever:
     verified-divergence  ns-sym fn-sym runs -> nil | message
   One macro:
     deftrifecta-verified name subject :runs n -> a deftest asserting no divergence"
  (:require [clojure.test]
            [ansatz.malli :as am]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn verified-divergence
  "nil when the compiled `fn-sym` in `ns-sym` agrees with the kernel evaluator
   across `runs` schema-generated inputs; else a message describing the first
   divergence (or a missing-schema / unencodable-value error). Wraps
   ansatz.malli/check-verified!, which THROWS on divergence — caught here and
   turned into a value so callers stay in the nil/violation-message idiom used by
   the other hive-schemas oracles (output-oracle, contract-violation)."
  [ns-sym fn-sym runs]
  (try
    (let [{r :runs o :ok} (am/check-verified! ns-sym fn-sym :runs runs)]
      (when (not= o r)
        (str "differential: only " o "/" r " runs agreed")))
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (str (.getMessage e)
             (when-let [args (:args d)] (str " — args " (pr-str args)))
             (when (contains? d :runtime)
               (str " (runtime " (pr-str (:runtime d)) " ≠ kernel " (pr-str (:kernel d)) ")")))))))

(defmacro deftrifecta-verified
  "Emit `name` — a deftest asserting the `a/defn`'d `subject` (a bare ns/fn
   symbol or #'ns/fn) is differentially faithful to the kernel across `:runs`
   generated inputs (default 25). The subject MUST be defined with `a/defn` and
   carry an `m/=>` schema over Nat/Bool/List Nat; otherwise the emitted test
   fails with a missing-schema message rather than throwing at macro time."
  [name subject & {:keys [runs] :or {runs 25}}]
  (let [subj (if (and (seq? subject) (= 'var (first subject))) (second subject) subject)
        nsq  (symbol (namespace subj))
        fnq  (symbol (clojure.core/name subj))]
    `(clojure.test/deftest ~name
       (let [v# (verified-divergence '~nsq '~fnq ~runs)]
         (clojure.test/is (nil? v#) (str v#))))))
