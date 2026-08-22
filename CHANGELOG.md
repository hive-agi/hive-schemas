# Changelog

Versions before 0.1.19 predate this file; consult `git log` for those.

## 0.1.20

### Added

- **`hive-schemas.test/deftrifecta-from-multi`** — a contract path for
  MULTIMETHODS. `fn?` is false for a multimethod (`MultiFn` implements `IFn`
  but not `clojure.lang.Fn`) and malli's `:=>` validator demands `fn?`, so an
  `m/=>` on a dispatch seam fails `malli.instrument/check` unconditionally,
  whatever the behaviour. The macro takes the arglist schema as an ordinary
  value and synthesizes seven facets:

  | facet | asserts |
  |---|---|
  | `-is-a-dispatch-seam` | subject is a multimethod, `:args` is an arglist schema, and no `m/=>` is registered for it |
  | `-vocabulary-is-closed` | `:dispatch` admits a non-empty closed set |
  | `-covers-the-vocabulary` | every declared value has a method |
  | `-has-no-default-method` | no `:default` catch-all (`:total?`, default true) |
  | `-dispatch-stays-in-vocabulary` | every argument list `:args` admits dispatches inside the vocabulary |
  | `-args-reach-the-vocabulary` | the seeded sample actually reaches every declared value |
  | `-conformance` | output conforms (when `:out` is given) |

  The vocabulary is read from `:dispatch`, never from the subject's own
  `defmethod` table — a totality check whose universe is derived from the
  methods it is checking passes by construction.

- Runtime levers `multimethod?`, `dispatch-fn`, `dispatch-vocabulary` and
  `undispatched`. `dispatch-vocabulary` answers `nil` for an OPEN schema
  (`:keyword`, `:any`, an `:or` with an open branch) rather than an empty
  vector, so a caller that gates on one fails loud instead of asserting
  nothing.

- A clj-kondo hook so consumers' linters see the generated vars.

## 0.1.19

### Breaking

- **`deftriad-from-schema` / `deftrifecta-from-schema`: a `:model-check` facet
  now FAILS when the model checker is unreachable.** Previously, if
  `hive-recife.core/check!` could not be resolved (dependency absent, or a cljs
  build where it cannot run), the emitted `-model-check` deftest passed
  silently — a facet that was never executed reported green.

  It now fails with a message naming the cause. To accept an unchecked facet
  deliberately, pass `:optional? true`:

  ```clojure
  {:model-check {:model-spec my.ns/spec
                 :optional?  true}}   ; green when recife is unreachable
  ```

  Consumers passing `:model-check` without `:optional? true` and relying on the
  old silent green will go red. That is the intent: a skipped verification is
  not a passed one. Add the `hive-recife` dependency, or opt in to skipping.

### Added

- **`hive-schemas.test/scalar-mutants`** — mutant synthesis for NON-MAP output
  schemas. `schema-mutants` previously derived mutants only from a map output's
  required entries, so a subject returning an `:enum`, a bounded `:int`, or any
  other scalar yielded an empty mutant set and its `-mutants-present` guard
  failed. `schema-mutants` now delegates to `scalar-mutants` when the output has
  no required map entries, emitting constant-return mutants the schema provably
  rejects: an out-of-vocabulary value, and `min-1` / `max+1` where the schema
  declares bounds.

### Fixed

- Bound lookups now resolve through registry-key, `:ref`, and `:schema`
  indirection to a fixpoint. `(m/properties (reg/schema ::priority))` is `nil`
  for a registered schema — its `:min`/`:max` live one `m/deref` down — so a
  registered bounded schema previously produced no bound mutants.
