# hive-schemas

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/hive-schemas.svg)](https://clojars.org/io.github.hive-agi/hive-schemas)

**A [malli](https://github.com/metosin/malli) schema drives property, mutation,
contract, characterization — and optionally differential — tests, with no
hand-written generator, oracle, or mutant.**

You pin a function's input/output schemas (and, ideally, a relation between
them); the bridge synthesizes the tests. The schema is the single source of
truth — the generator, the conformance oracle, and the mutants are all *derived*
from it. Your test namespace never sees malli directly.

```clojure
(require '[hive-schemas.test :as hst]
         '[hive-spi.schema.registry :as reg])

(reg/register! ::in  [:map [:x [:int {:min -10000 :max 10000}]]
                           [:y [:int {:min -10000 :max 10000}]]])
(reg/register! ::out [:map [:sum :int] [:product :int]])

(defn calc [{:keys [x y]}] {:sum (+ x y) :product (* x y)})

(defn calc-rel [in out]
  (and (= (:sum out)     (+ (:x in) (:y in)))
       (= (:product out) (* (:x in) (:y in)))))

;; One form -> five synthesized test vars (conformance, relation, contract,
;; mutants-present, mutations).
(hst/deftrifecta-from-schema calc-tests calc
  {:in ::in :out ::out :rel calc-rel :contract true})
```

## Install

```clojure
;; deps.edn
io.github.hive-agi/hive-schemas {:mvn/version "0.1.1"}
```

The `hive-schemas.test` synthesis bridge additionally needs
[`hive-test`](https://clojars.org/io.github.hive-agi/hive-test) (the mutation /
golden machinery it emits into):

```clojure
io.github.hive-agi/hive-test {:mvn/version "0.3.0"}
```

Everything else — `malli`, `test.check`, and `hive-spi` (the schema registry) —
comes transitively.

## Why

Conformance alone is a **weak** oracle: it only checks the output *shape*. Two
things give the synthesized suite teeth:

1. **Pin `:out` tightly** — e.g. `[:and ::node [:map [:k [:= v]]]]`, not just
   `:map`.
2. **Pass a `:rel`** — a `(fn [in out] boolean)` relating output to input.

With those, the same schema yields a generator, an oracle, a behavioral
contract, and a set of *sound* mutants (values the schema provably rejects), all
for free.

## `deftrifecta-from-schema`

`(deftrifecta-from-schema name subject opts)` — `subject` is a bare `ns/fn`
symbol (or `#'ns/fn`).

| opt           | meaning                                                            |
|---------------|-------------------------------------------------------------------|
| `:in`         | input schema (registry key or malli form) — **required**          |
| `:out`        | output schema — **required**                                      |
| `:rel`        | `(fn [in out] boolean)` — optional, **strongly recommended**       |
| `:idempotent?`| assert `(= (f (f x)) (f x))`                                       |
| `:contract`   | emit a malli-native `mg/check` facet (rung B)                     |
| `:mutation`   | emit the mutation facet + non-vacuity guard (default `true`)       |
| `:golden-path`| snapshot `{case -> {:in :out}}` over seeded cases to an EDN file   |
| `:num-tests`  | property iterations (default 100)                                 |
| `:seed`       | seed for the deterministic mutation/golden cases (default 0)       |
| `:n-cases`    | number of seeded cases (default 8)                               |
| `:strict-in`  | fail when `:in` accepts everything (rung-A honesty; opt-in)        |

### Synthesized facets (each a distinct test var)

| var                      | checks                                                        |
|--------------------------|--------------------------------------------------------------|
| `<name>-conformance`     | `∀ in ~ :in`, `(subject in)` conforms to `:out`               |
| `<name>-relation`        | `∀ in`, `(rel in (subject in))`               *(when `:rel`)*  |
| `<name>-idempotent`      | `∀ in`, `subject` is idempotent        *(when `:idempotent?`)* |
| `<name>-contract`        | `mg/check` of `[:=> [:cat :in] :out rel]`  *(when `:contract`)*|
| `<name>-mutants-present` | **fails loud** if `:out` yields no mutants *(when `:mutation`)*|
| `<name>-mutations`       | every schema-derived mutant is caught      *(when `:mutation`)*|
| `<name>-golden`          | outputs match the stored snapshot        *(when `:golden-path`)*|
| `<name>-input-strength`  | `:in` is not vacuous                      *(when `:strict-in`)* |

The **non-vacuity guard** (`<name>-mutants-present`) is deliberate: a mutation
suite that generates zero mutants is silently vacuous, so the bridge fails
rather than pass an empty facet. If `:out` is genuinely too permissive to
corrupt soundly, tighten it or pass `:mutation false`.

### Sound mutants

Mutants are only ever values the schema **provably rejects** — a dropped
required key, or a wrong-typed value the key's schema refuses. The bridge
resolves the output schema through `:and`, `:maybe`, registry refs, and
`:or`/`:multi` unions:

- `[:maybe M]` → derefs to `M` (nil stays valid; a corrupted non-nil map is
  neither).
- `[:or M1 … Mn]` / `[:multi …]` → only keys required in **every** branch are
  corruptible, and every branch must itself be a map — otherwise a permissive
  branch could re-accept the corrupted value, so the bridge honestly declines.

## `deftrifecta-predicate`

For a plain predicate + a schema, `(deftrifecta-predicate name pred {:schema s})`
synthesizes:

- `<name>-positive` — `∀ x ~ schema`, `(true? (pred x))`
- `<name>-negative` — every schema-corruption of a valid sample is rejected
  (kills both `(constantly true)` and `(constantly false)`).

## Runtime levers

The macros are thin; the derivations are plain functions you can call directly:

| lever                  | `?s ->`                                                    |
|------------------------|------------------------------------------------------------|
| `input-gen`            | a `test.check` generator                                  |
| `output-oracle`        | `x -> boolean` (SO-safe; recursive-schema friendly)       |
| `required-entries`     | `[[k child] …] \| nil` (`:and`/`:maybe`/`:or`/`:multi`)     |
| `wrong-value-for`      | a value the child schema rejects, or `::unfalsifiable`     |
| `schema-mutants`       | `orig ?out -> [[label mutant-fn] …]` (sound)              |
| `schema-corruptions`   | `?s v -> [[label corrupted-v] …]`                         |
| `seeded-cases`         | a reproducible, sorted sample of inputs                    |
| `contract-violation`   | `nil \| message` via `mg/check` (rung B)                   |

## Two axes, not one ladder

Evidence about a subject varies along **two independent axes**. Read a facet's
position on both before reporting what it establishes.

**Axis 1 — strength of the evidence about one function.** Each rung is strictly
stronger than the last:

| rung  | facet                       | what it establishes                               |
|-------|-----------------------------|---------------------------------------------------|
| **0** | `wire` / `instrument`       | the schema *runs* — coercion, round-trip, `m/=>`   |
| **A** | conformance                 | the output *shape* holds (sampled)                 |
| **B** | `:contract`                 | a relation holds, in-malli via `mg/check` (sampled)|
| **C** | mutation + golden           | the suite is adequate (kills sound mutants); the behavior is characterized |
| **D** | schema-as-type              | the schema *is* the type — checked by `:typed`     |
| **E** | `:ansatz` differential      | the compiled runtime ≡ an independent kernel evaluator (sampled) |
| **F** | `:ansatz` proof / `csimp`   | the property holds ∀ inputs, by construction       |

**Axis 2 — scope of the claim.** Orthogonal to rung; a claim can be strong and
narrow, or weak and wide:

| scope        | lever                                    | claims about              |
|--------------|------------------------------------------|---------------------------|
| one value    | `validate` / `explain`                   | a single datum            |
| one function | `deftrifecta-from-schema`                | one subject               |
| one **port** | `deftest-port-contract`                  | every adapter of a port   |
| one namespace| `deftest-contract-coverage`              | that nothing is uncovered |
| the registry | `deftest-schema-compat`                  | that no contract broke    |
| over time    | `:model-check` (recife/TLC)              | a state machine's traces  |

Rungs 0–C and every axis-2 lever are pure malli/test.check and ship in the core
+ `:test-synth` modules. Rungs D, E and F are opt-in modules (below).

**Sampled is not proven.** Rungs A, B, E and the compatibility checker generate
inputs; their messages say so. Only rung F quantifies over all inputs.

## Optional module — rung E (`hive-schemas.verified`)

The differential facet checks the **compiled runtime** of an
[`ansatz`](https://clojars.org/org.replikativ/ansatz)-defined function against
the Lean 4 CIC kernel's own evaluation, on schema-generated inputs. A divergence
is an *elaboration* bug — a well-typed but source-unfaithful program that a type
check alone cannot see. This is complementary to rungs A/B: those pin the output
*shape* and a relation you wrote; the differential lane pins the output *value*
against an independent evaluator of the same source.

```clojure
;; deps.edn — the :ansatz alias pulls the kernel
:ansatz {:extra-paths ["ansatz"]
         :extra-deps  {org.replikativ/ansatz {:mvn/version "<latest-version>"}}}
```

```clojure
(require '[hive-schemas.verified :refer [deftrifecta-verified]])
;; subject must be defined with ansatz's a/defn and carry an m/=> schema over
;; Nat / Bool / List Nat.
(deftrifecta-verified add2-verified my.ns/add2 :runs 50)
```

Scope: Nat / Bool / (List Nat) arguments and results (ansatz differential v1).
Map / keyword ops are opaque carriers in v1 — stay on the malli rungs for them.

## Plans — the macro is a projection, not the entry point

A **plan** is a value: subject, schemas, selected facets, provenance. Producers
that are not a human — hive-domain's `:hive-schemas` facet, a contract inferred
from traces — build plans, and the emitter renders them.

```clojure
(require '[hive-schemas.plan :as plan]
         '[hive-schemas.emit :as emit])

(def p (plan/plan 'calc-tests 'my.ns/calc
                  {:in ::in :out ::out :rel 'my.ns/calc-rel :contract true}))

(plan/facet-vars p)   ;; => [calc-tests-conformance calc-tests-relation ...]
(emit/render-ns {:ns 'my.ns.generated-test :plans [p]})   ;; => source text
(emit/spit-ns! "test" {:ns 'my.ns.generated-test :plans [p]})

;; :preamble forms render after the ns form and before the plans, for whatever
;; setup a generated suite needs before its subjects resolve.
(emit/render-ns {:ns       'my.ns.generated-test
                 :requires '[[hive-domain.core :as domain]]
                 :preamble '[(domain/install! spec)
                             (def conserved (domain/relation-predicate spec :a/law))]
                 :plans    [p]})
```

`hive-schemas.test/deftriad-from-plan` expands a plan in place. `:plan/provenance`
is `:declared` | `:inferred` | `:compiled`. `spit-ns!` refuses to overwrite unless
asked; `plan->form` refuses a `:rel` that is a compiled fn rather than a symbol.

## Contract coverage

```clojure
(require '[hive-schemas.coverage :refer [deftest-contract-coverage]])

(deftest-contract-coverage every-fn-is-contracted
  {:paths  ["src"]
   :exempt {'my.ns/legacy-thing "pending the 0.4 rewrite"}})
```

The universe is read from the **files**, not from `m/function-schemas`. Four
independent gates: the universe is non-empty; nothing was unreadable or failed to
load; every uncontracted function is exempted; every exemption carries a prose
reason and still applies.

## Rung 0 — the wire boundary

```clojure
(require '[hive-schemas.wire :refer [deftrifecta-wire]]
         '[hive-schemas.instrument :as inst])

(deftrifecta-wire op-in ::my-op-in {:roundtrip true})

(inst/instrument! {:ns 'my.ns})
(inst/check-violation {:ns 'my.ns})   ;; => nil | message
```

`instrument!` alters var roots; after a `:reload-all` the wrappers are orphaned
and a fresh JVM is required.

## Stubs, spies, and port contracts

```clojure
(require '[hive-schemas.stub :as stub]
         '[hive-schemas.port :refer [deftest-port-contract]])

(stub/stub ::out)                       ;; a deterministic conformant value
(stub/stub-fn ::in ::out)               ;; a fake that rejects bad input
(def spied (stub/spy my.ns/f {:in ::in :out ::out}))
(stub/violations spied)                 ;; calls whose ends did not conform
(stub/trace->cases (stub/calls spied))  ;; => golden :cases from real traffic

(deftest-port-contract datahike-store
  {:port/methods {get-it {:in [:cat ::k] :out ::v}}}
  (->DatahikeStore cfg))                ;; the adapter is INJECTED
```

`stub/default-provider` is the seam a structural stub (`hive-test.stub/defstub`)
calls to fill a method with a schema-conformant value instead of nil.

## Schema strength

```clojure
(require '[hive-schemas.strength :as strength])

(strength/schema-strength ::in)   ;; => {:rejection-rate 0.87 :degenerate? false ...}
(strength/input-vacuity :any)     ;; => "vacuous :in — ..."
(strength/type-degeneracy [:fn even?])  ;; => "degenerate type projection — ..."
```

Measured against a fixed value ladder, so scores are comparable across schemas
and stable across runs.

## Schema evolution

```clojure
(require '[hive-schemas.evolution :as evo])

(evo/compat-violation old new {:variance :input})   ;; inputs may WIDEN
(evo/compat-violation old new {:variance :output})  ;; outputs may NARROW
(evo/breaking-changes old-snapshot (evo/registry-snapshot) {})
```

Golden the registry to see contract drift in review:

```clojure
(hive-test.golden/deftest-golden-fn registry-snapshot
  "test/golden/registry.edn" evo/registry-snapshot)
```

## Modules & aliases

| alias         | source root | adds                                             |
|---------------|-------------|--------------------------------------------------|
| *(core)*      | `src/`      | registry + derivation levers, `vocab`, `plan`/`emit`, `coverage`, `wire`, `instrument`, `strength`, `stub`, `port`, `evolution` |
| `:test-synth` | `synth/`    | the `hive-schemas.test` bridge (pulls `hive-test`) |
| `:typed`      | `typed/`    | the rung-D `hive-schemas.typed-check` ns (pulls `typed.clj.checker`) |
| `:ansatz`     | `ansatz/`   | the rung-E `hive-schemas.verified` / `proven` nses (pulls `ansatz`) |
| `:local`      | —           | dev: override `hive-spi`/`hive-test` with sibling working copies |

Every root ships in the jar at the path its namespace resolves at; the heavy dep
each optional layer needs is the consumer's to add. Core requires none of them.

`:local` is for co-developing the sibling libraries without a
`clojure -T:build install` round-trip; it composes with any alias, e.g.
`clojure -M:test:local`.

## ClojureScript

The bridge is `.cljc` and pure codegen. `deftrifecta-from-schema` /
`deftrifecta-predicate` emit `deftest`/`is` per platform (via `&env`); every
other emitted symbol (`defspec`, `for-all`, mutation, golden) lives in a
cross-platform namespace, so the synthesized suites run on both clj and cljs.

## License

MIT © 2026 Pedro Gomes Branquinho (BuddhiLW) &lt;pedrogbranquinho@gmail.com&gt;
