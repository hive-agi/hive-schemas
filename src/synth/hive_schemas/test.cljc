(ns hive-schemas.test
  "Schema-driven test synthesis.

   A malli schema drives hive-test property + mutation tests with no
   hand-written generator, oracle, or mutant. hive-test never sees malli.

   Two synthesis macros:
     deftrifecta-from-schema  in/out schematized fn -> conformance + (optional
                              relational / idempotence) property + mutation
     deftrifecta-predicate    a predicate + a schema -> positive (valid inputs
                              accepted) + negative (corrupted inputs rejected)

   Conformance alone is a WEAK oracle (it only checks the output SHAPE). For
   real teeth: pin the :out schema (e.g. [:and ::node [:map [:k [:= v]]]]) and
   pass a :rel (fn [in out] ...) that relates output to input.

   The classic hive-test.trifecta/deftrifecta composes with the levers too:
   source :gen/:pred/:cases from input-gen/output-oracle/seeded-cases calls in
   the literal spec map — golden + property + mutation facets then run entirely
   off the registered schema.

   Runtime levers:
     input-gen         ?in  -> test.check generator
     output-oracle     ?out -> (x -> boolean)      (m/validator; SO-safe)
     required-entries  ?s   -> [[k child] ...] | nil (descends :and/:maybe/:ref/
                                :schema; :or/:multi -> keys required in every branch)
     wrong-value-for   child -> a value the child REJECTS, or ::unfalsifiable
     schema-mutants    orig ?out -> [[label mutant-fn] ...] (sound; skips unfalsifiable)
     schema-corruptions ?s v    -> [[label corrupted-v] ...]
     seeded-cases      ?in seed n -> {label input} (deterministic)
     contract-violation ?in ?out rel f -> nil | violation-msg (mg/check; rung B)"
  (:require [hive-spi.schema.registry :as reg]
            [hive-spi.schema.gen :as sgen]
            [hive-test.mutation :as mut]
            [hive-test.mutation.combinators :as mc]
            [hive-test.golden :as golden]
            [clojure.set :as set]
            ;; test.check's defspec / for-all are MACROS in cross-platform .cljc
            ;; namespaces; the aliases let the syntax-quoted emissions resolve to
            ;; fully-qualified symbols at compile time on BOTH clj and cljs.
            [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg])
  ;; Self-require macros so cljs consumers pull deftrifecta-from-schema /
  ;; -predicate via plain :require/:refer. The macros are pure codegen: the only
  ;; platform-specific symbols they emit are deftest / is (chosen via (:ns &env)
  ;; below); defspec / for-all / golden.* / mutation.* live in .cljc, so their
  ;; fully-qualified emissions resolve identically on clj and cljs.
  #?(:cljs (:require-macros [hive-schemas.test])))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; Schema resolution + oracle
;; =============================================================================

(defn input-gen
  "test.check generator for `?in-schema` (registry key or malli form)."
  [?in-schema]
  (sgen/generator ?in-schema))

(defn output-oracle
  "Predicate x -> boolean: does x conform to `?out-schema`. Uses m/validator
   directly (not compile-op) so RECURSIVE schemas stay SO-safe — validation
   handles self-reference, whereas the JSON-Schema / Typed-type derivations in
   compile-op recurse without a base case."
  [?out-schema]
  (m/validator (reg/schema ?out-schema)))

(defn- deref-toward
  "Deref/descend `?schema` one level at a time to a fixpoint, toward a STRUCTURAL
   schema (:map, :or, :multi). SO-safe on RECURSIVE schemas (unlike m/deref-all).
     :maybe -> its single child   (nullable wrapper; the map underneath carries
                                    the corruptible keys)
     :and   -> first structurally-resolving child (the oracle still enforces
                                    every conjunct, so a corruption violating one
                                    conjunct violates the whole :and)
     :ref / :schema / registry-key -> m/deref
   Returns the structural schema, or nil when `?schema` doesn't resolve to one."
  [?schema]
  (loop [s (reg/schema ?schema) n 0]
    (cond
      (#{:map :or :multi} (m/type s)) s
      (> n 32)              nil
      (= :maybe (m/type s)) (recur (first (m/children s)) (inc n))
      (= :and (m/type s))   (some deref-toward (m/children s))
      :else                 (let [d (m/deref s)]
                              (when-not (= (m/form d) (m/form s))
                                (recur d (inc n)))))))

(defn- map-entries
  "[[k child-schema] ...] for the non-optional entries of a :map schema."
  [s]
  (vec (for [[k props child] (m/children s)
             :when (not (:optional props))]
         [k child])))

(defn- branch-schemas
  "The alternative branch schemas of an :or (children as-is) or :multi (the
   schema of each [dispatch-val props schema] entry)."
  [s]
  (case (m/type s)
    :or    (m/children s)
    :multi (map #(nth % 2) (m/children s))))

(defn- union-entries
  "Sound required entries for a UNION of `branches` (an :or/:multi). A key is
   corruptible iff it is required in EVERY branch — dropping it (or setting a
   value every branch rejects) then provably violates all branches, hence the
   union. Returns nil unless every branch resolves to a :map: a non-map or
   permissive branch could ACCEPT the corrupted value, so no sound intersection
   exists. `exclude` drops keys we must not touch (a :multi dispatch key, whose
   corruption merely re-routes the value to another branch). The mutant child of
   a shared key is the disjunction of its per-branch child schemas, so
   wrong-value-for must find a value REJECTED by every branch."
  [branches exclude]
  (let [maps (map deref-toward branches)]
    (when (and (seq maps) (every? #(and % (= :map (m/type %))) maps))
      (let [entryv   (mapv map-entries maps)
            common   (apply disj
                            (reduce set/intersection (map #(set (map first %)) entryv))
                            exclude)
            child-of (fn [k] (into [:or] (for [es entryv
                                               [ek c] es
                                               :when (= ek k)]
                                           (m/form c))))]
        (vec (for [k common] [k (child-of k)]))))))

(defn required-entries
  "[[k child-schema] ...] for the SOUND corruptible output keys of `?schema` —
   keys whose drop / wrong-type provably violates it. For a :map: its
   non-optional entries. For an :or/:multi over maps: the keys required in every
   branch, each child the disjunction across branches. nil when `?schema` doesn't
   resolve to a :map or a union of maps. Descends :and, :maybe, :ref, :schema,
   and registry refs."
  [?schema]
  (when-let [s (deref-toward ?schema)]
    (case (m/type s)
      :map   (map-entries s)
      :or    (union-entries (branch-schemas s) nil)
      :multi (let [dk (-> s m/properties :dispatch)]
               ;; only a keyword dispatch is analyzable; a fn dispatch reads
               ;; unknown fields, so we can't guarantee corruption soundness
               (when (keyword? dk)
                 (union-entries (branch-schemas s) #{dk})))
      nil)))

;; =============================================================================
;; Mutants / corruptions (sound: proven-rejecting values only)
;; =============================================================================

(def ^:private wrong-value-ladder
  "Type-distinct candidate values; the first REJECTED by a key's schema becomes
   its wrong-type mutant value."
  [::wrong-type "hive-schemas/x" -1 true {} [] nil])

(defn wrong-value-for
  "A value the child schema `c` provably REJECTS (so an assoc of it violates the
   key), or ::unfalsifiable when `c` accepts every ladder candidate (e.g. :any)."
  [c]
  (let [cv (m/validator (reg/schema c))]
    (if-let [v (first (remove cv wrong-value-ladder))]
      v
      ::unfalsifiable)))

(defn schema-mutants
  "[[label mutant-fn] ...] — for each required OUTPUT key of `?out-schema`, a
   drop-key mutant (a dropped required key always violates -> a guaranteed kill)
   and, WHEN a rejecting value exists, a wrong-type mutant. Keys whose schema
   accepts everything (:any) get drop-key only, never a degenerate wrong-type
   that would silently survive. Each mutant WRAPS `orig` (the real fn value,
   captured before any var rebind)."
  [orig ?out-schema]
  (vec
    (mapcat
      (fn [[k child]]
        (let [wrong (wrong-value-for child)]
          (cond-> [(mut/as-pair (mc/drop-key orig k))]
            (not= wrong ::unfalsifiable)
            (conj (mut/as-pair (mc/assoc-const orig k wrong))))))
      (required-entries ?out-schema))))

(defn schema-corruptions
  "[[label corrupted-value] ...] — apply each required-key drop and (where a
   rejecting value exists) wrong-type to `v`. Every corrupted value violates
   `?schema`, so a correct predicate MUST reject it."
  [?schema v]
  (vec
    (mapcat
      (fn [[k child]]
        (let [wrong (wrong-value-for child)]
          (cond-> [[(str "drop-" k) (dissoc v k)]]
            (not= wrong ::unfalsifiable)
            (conj [(str "wrong-type-" k) (assoc v k wrong)]))))
      (required-entries ?schema))))

(defn seeded-cases
  "{label input} — a reproducible, sorted sample of `n` inputs drawn from
   `?in-schema`. Same seed yields the same cases. Delegates to
   hive-spi.schema.gen/seeded-cases (the derivation lever's :cases projection);
   the coherence test guards the delegation."
  [?in-schema seed n]
  (sgen/seeded-cases ?in-schema seed n))

;; =============================================================================
;; Contract — malli-native behavioral spec (mg/check over a :=> function schema)
;; =============================================================================

(defn contract-schema
  "A malli function schema [:=> [:cat ?in] ?out guard] bound to the hive-spi
   registry (so registry-ref inputs/outputs resolve). Models a SINGLE-arg
   subject: the guard receives [[in] ret], so a relation `rel` of (fn [in out])
   is adapted to (fn [[[i] r]] (rel i r)). With no `rel` the schema still
   pins the OUTPUT, so mg/check subsumes an output-conformance check."
  [?in ?out rel]
  (reg/schema [:=> [:cat ?in] ?out
               (if rel [:fn (fn [[[i] r]] (rel i r))] :any)]))

(defn contract-violation
  "nil when `subject` satisfies the [:=> ?in ?out guard] contract across
   malli-generated inputs; else a message naming the SHRUNK smallest failing
   input and the output it produced. Wraps malli.generator/check — malli's own
   generative function checker, so output-shape AND the relation are verified in
   one pass (ladder rung B, in-malli behavioral)."
  [?in ?out rel subject]
  (when-let [f (mg/check (contract-schema ?in ?out rel) subject)]
    (let [chk (-> f :errors first :check)
          in  (ffirst (:smallest chk))
          ret (get chk :malli.core/result)]
      (str "contract violated — input " (pr-str in) " -> " (pr-str ret)))))

;; =============================================================================
;; Codegen
;; =============================================================================

(defn- subject-sym
  "Normalize a subject to the bare qualified symbol. Accepts `ns/fn` or `#'ns/fn`."
  [subject]
  (if (and (seq? subject) (= 'var (first subject)))
    (second subject)
    subject))

(defmacro deftrifecta-from-schema
  "Synthesize tests for `subject` (a bare `ns/fn` symbol or `#'ns/fn`) from its
   malli input/output schemas.

   opts:
     :in          input schema (registry key or malli form)      [required]
     :out         output schema (registry key or malli form)     [required]
     :rel         (fn [in out] boolean) relating output to input [optional but
                  RECOMMENDED — conformance alone rarely has teeth]
     :idempotent? emit (= (subject (subject x)) (subject x))      [optional]
     :contract    emit a malli-native mg/check facet over a [:=> [:cat :in] :out
                  :fn-of-:rel] function schema (rung B)           [optional]
     :mutation    emit the mutation facet + non-vacuity guard     (default true)
     :golden-path relative EDN path — snapshot {case -> {:in :out}} over the
                  seeded cases and characterize it (opt-in)       [optional]
     :num-tests   property iterations                            (default 100)
     :seed        mutation input seed                            (default 0)
     :n-cases     mutation input cases                           (default 8)

   Facets emitted (each a distinct var):
     <name>-conformance     for all in ~ :in, output conforms to :out
     <name>-relation        for all in ~ :in, (rel in (subject in))   [when :rel]
     <name>-idempotent      for all in ~ :in, subject is idempotent   [when :idempotent?]
     <name>-contract        mg/check of the [:=> in out :fn-of-rel] schema [when :contract]
     <name>-mutants-present FAILS LOUD if :out yields no mutants       [when :mutation]
     <name>-mutations       each schema-derived mutant is caught       [when :mutation]
     <name>-golden          snapshot of the seeded cases' outputs      [when :golden-path]"
  [name subject {:keys [in out rel idempotent? contract mutation golden-path
                        num-tests seed n-cases]
                 :or {mutation true num-tests 100 seed 0 n-cases 8}}]
  (let [subj        (subject-sym subject)
        gsym        (gensym "gen")
        psym        (gensym "oracle")
        csym        (gensym "cases")
        is-sym      (if (:ns &env) 'cljs.test/is 'clojure.test/is)
        deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)]
    `(do
       (def ~gsym (input-gen ~in))
       (def ~psym (output-oracle ~out))
       (def ~csym (seeded-cases ~in ~seed ~n-cases))
       (tc/defspec ~(symbol (str name "-conformance")) ~num-tests
         (prop/for-all [in# ~gsym]
           (~psym (~subj in#))))
       ~@(when rel
           [`(tc/defspec ~(symbol (str name "-relation")) ~num-tests
               (prop/for-all [in# ~gsym]
                 (~rel in# (~subj in#))))])
       ~@(when idempotent?
           [`(tc/defspec ~(symbol (str name "-idempotent")) ~num-tests
               (prop/for-all [in# ~gsym]
                 (= (~subj (~subj in#)) (~subj in#))))])
       ~@(when contract
           [`(~deftest-sym ~(symbol (str name "-contract"))
               (let [v# (contract-violation ~in ~out ~rel ~subj)]
                 (~is-sym (nil? v#) (str v#))))])
       ~@(when mutation
           [`(~deftest-sym ~(symbol (str name "-mutants-present"))
               (~is-sym (pos? (count (schema-mutants ~subj ~out)))
                        "no schema-derived mutants — :out too permissive; tighten :out or pass :mutation false"))
            `(mut/deftest-mutations ~(symbol (str name "-mutations"))
               ~subj
               (schema-mutants ~subj ~out)
               (fn []
                 (doseq [in# (vals ~csym)]
                   (~is-sym (~psym (~subj in#))
                            (str "schema-driven: output violates :out for input " in#)))))])
       ~@(when golden-path
           [`(golden/deftest-golden-fn ~(symbol (str name "-golden"))
               ~golden-path
               (fn [] (into (sorted-map)
                            (map (fn [[label# in#]] [label# {:in in# :out (~subj in#)}]))
                            ~csym)))]))))

(defmacro deftrifecta-predicate
  "Synthesize positive + negative tests for a PREDICATE `subject` against a
   `:schema`. Kills both (constantly true) and (constantly false):

     <name>-positive  for all x ~ generator(:schema), (true? (subject x))
     <name>-negative  for each schema-corruption of a valid sample,
                      (false? (subject x))

   opts: :schema [required], :num-tests (100), :seed (0), :n-cases (8)."
  [name subject {:keys [schema num-tests seed n-cases]
                 :or {num-tests 100 seed 0 n-cases 8}}]
  (let [subj        (subject-sym subject)
        gsym        (gensym "gen")
        is-sym      (if (:ns &env) 'cljs.test/is 'clojure.test/is)
        deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)]
    `(do
       (def ~gsym (input-gen ~schema))
       (tc/defspec ~(symbol (str name "-positive")) ~num-tests
         (prop/for-all [x# ~gsym]
           (true? (~subj x#))))
       (~deftest-sym ~(symbol (str name "-negative"))
         (let [goods# (vals (seeded-cases ~schema ~seed ~n-cases))]
           (~is-sym (seq (mapcat #(schema-corruptions ~schema %) goods#))
                    "no corruptions derivable — schema too permissive for a predicate negative test")
           (doseq [good# goods#
                   [label# bad#] (schema-corruptions ~schema good#)]
             (~is-sym (false? (~subj bad#))
                      (str "predicate accepted a schema-corrupted value: " label#))))))))

(defmacro deftriad-from-schema
  "TRIAD-IN-ONE: the verification ladder from ONE registered schema, in one entry.
   Emits the malli facets of `deftrifecta-from-schema` (conformance A / contract B
   / mutation C / generative D) and, on demand, the two legs malli alone cannot
   give:
     :prove       {:params p :prop q :tactics t}  an ansatz kernel PROOF (rung F)
                  via hive-schemas.proven/deftrifecta-proven — the malli+ansatz
                  triad off ONE schema. Emitted as a fully-qualified form that
                  expands in the CONSUMER's ns, so THIS ns stays ansatz-free at
                  load: a :prove consumer adds the :ansatz alias and requires
                  hive-schemas.proven. JVM-only (omitted on cljs).
     :model-check {:model-spec s}  a recife TLA+/TLC facet (orthogonal). recife is
                  resolved LAZILY at run time (requiring-resolve, guarded — an
                  absent ns THROWS, so the guard turns that into nil), so the
                  facet SKIPS green when recife is absent and adds no load-time
                  dep. JVM-only (a cljs consumer gets a trivial skip).
   All other opts pass through to `deftrifecta-from-schema` unchanged. The proof
   and model-check facets are decoupled by construction (DIP): one registered
   schema shapes the malli facets AND the ansatz a/defn the proof reasons about."
  [name subject {:keys [prove model-check] :as opts}]
  (let [base (dissoc opts :prove :model-check)
        cljs? (boolean (:ns &env))]
    `(do
       (deftrifecta-from-schema ~name ~subject ~base)
       ~@(when (and prove (not cljs?))
           [`(hive-schemas.proven/deftrifecta-proven
               ~(symbol (str name "-proof"))
               :params ~(:params prove) :prop ~(:prop prove) :tactics ~(:tactics prove))])
       ~@(when model-check
           [(if cljs?
              `(cljs.test/deftest ~(symbol (str name "-model-check"))
                 (cljs.test/is true "model-check is JVM-only — skipped on cljs"))
              `(clojure.test/deftest ~(symbol (str name "-model-check"))
                 (if-let [check!# (try (requiring-resolve 'hive-recife.core/check!)
                                       (catch Throwable _# nil))]
                   (let [r# (check!# ~(:model-spec model-check))]
                     (clojure.test/is (not= :fail (:status r#))
                                      (str "model-check refuted: " (pr-str (:details r#)))))
                   (clojure.test/is true "hive-recife absent — model-check skipped"))))]))))