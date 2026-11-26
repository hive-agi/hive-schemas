(ns hive-schemas.test-support
  "Shared fixtures for the kernel suites under test-ansatz/ (-M:ansatz-test only).

   Every rung-E/F test namespace needs the same set-up: boot the ansatz kernel
   stdlib, then elaborate its subject (an `a/defn`, sometimes after its `m/=>`
   registration) into the live env AND the namespace's clojure var. This is
   that set-up, once.

   Levers:
     boot!           ns-sym -> boots the kernel stdlib, once per namespace
     kernel-has?     const-name -> is that constant in the live env
     ensure-subject! ns-sym const-name forms -> evals `forms` in `ns-sym` unless
                     the kernel env already holds `const-name`"
  (:require [ansatz.core :as a]
            [ansatz.kernel.env :as env]
            [ansatz.kernel.name :as kname]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defonce ^:private booted-nss
  (atom #{}))

(defn boot!
  "Run `a/load-init!` the first time `ns-sym` asks, and never again for it. The
   boot is per namespace, as each suite's own `defonce` delay was: load-init!
   re-inits the env, so a namespace that boots owns the subjects it then
   elaborates, whatever order kaocha runs the suites in. Returns nil."
  [ns-sym]
  (let [[old _] (swap-vals! booted-nss conj ns-sym)]
    (when-not (contains? old ns-sym)
      (binding [a/*verbose* false] (a/load-init!))))
  nil)

(defn kernel-has?
  "True when the live kernel env holds the constant named `const-name`."
  [const-name]
  (some? (env/lookup (a/env) (kname/from-string const-name))))

(defn ensure-subject!
  "Boot the kernel for `ns-sym`, then eval `forms` (quoted data, in order) with
   `*ns*` bound to `ns-sym`, unless the env already holds `const-name`. Order
   matters: an `m/=>` registration must precede the `a/defn` that reads it.
   Idempotent per kernel env. Returns nil; an elaboration error is thrown."
  [ns-sym const-name forms]
  (boot! ns-sym)
  (binding [a/*verbose* false]
    (when-not (kernel-has? const-name)
      (binding [*ns* (the-ns ns-sym)]
        (doseq [f forms] (eval f)))))
  nil)

(defn subject-fixture
  "A clojure.test :once fixture that runs `ensure-subject!` for each
   `[const-name forms]` pair in `subjects`, in order, before the tests of
   `ns-sym`."
  [ns-sym & subjects]
  (fn [t]
    (doseq [[const-name forms] subjects]
      (ensure-subject! ns-sym const-name forms))
    (t)))
