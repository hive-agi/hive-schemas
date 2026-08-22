# Changelog

Versions before 0.1.19 predate this file; consult `git log` for those.

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
