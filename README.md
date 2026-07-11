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

## The verification ladder

The facets are rungs on a ladder of increasing rigor — each strictly stronger
than the last:

| rung  | facet                       | what it proves                                    |
|-------|-----------------------------|---------------------------------------------------|
| **A** | conformance                 | the output *shape* holds (probabilistically)       |
| **B** | `:contract`                 | a relation holds, in-malli via `mg/check`          |
| **C** | mutation + golden           | the suite is adequate (kills sound mutants); the behavior is characterized |
| **D** | schema-as-type              | the schema *is* the type (derivation)              |
| **E** | `:ansatz` differential      | the compiled runtime ≡ an independent kernel evaluator |

Rungs A–C are pure malli/test.check and ship in the core + `:test-synth`
modules. Rung E is opt-in (below).

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
:ansatz {:extra-paths ["src/ansatz"]
         :extra-deps  {org.replikativ/ansatz {:mvn/version "0.2.75"}}}
```

```clojure
(require '[hive-schemas.verified :refer [deftrifecta-verified]])
;; subject must be defined with ansatz's a/defn and carry an m/=> schema over
;; Nat / Bool / List Nat.
(deftrifecta-verified add2-verified my.ns/add2 :runs 50)
```

Scope: Nat / Bool / (List Nat) arguments and results (ansatz differential v1).
Map / keyword ops are opaque carriers in v1 — stay on the malli rungs for them.

## Modules & aliases

| alias         | adds                                                          |
|---------------|--------------------------------------------------------------|
| *(core)*      | schema registry + derivation levers (`hive_schemas.schema`)   |
| `:test-synth` | the `hive-schemas.test` bridge (pulls `hive-test`)            |
| `:ansatz`     | the rung-E `hive-schemas.verified` ns (pulls `ansatz`)        |
| `:local`      | dev: override `hive-spi`/`hive-test` with sibling working copies |

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
