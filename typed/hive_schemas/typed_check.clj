(ns hive-schemas.typed-check
  "OPTIONAL rung-D facet: the Typed Clojure checker over a namespace.

   Loads only when org.typedclojure/typed.clj.checker is on the classpath (the
   :typed alias). Complements `hive-schemas.strength/type-degeneracy`, which
   reports a projection that lost its information without needing the checker.

   Levers:
     type-errors      ns-sym -> [type-error ...]
     check-violation  ns-sym -> nil | message
   One macro:
     deftest-typed-check  name ns-sym -> a deftest asserting the ns checks clean"
  (:require [clojure.test]
            [typed.clj.checker :as checker]
            [hive-schemas.vocab :as vocab]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn type-errors
  "Type errors the checker reports for `ns-sym`."
  [ns-sym]
  (:type-errors (checker/check-ns-info ns-sym)))

(defn opted-in?
  "True when `ns-sym` carries the `^:typed.clojure` ns metadata the checker
   requires before it will check a namespace at all. Loads `ns-sym`."
  [ns-sym]
  (require ns-sym)
  (boolean (:typed.clojure (meta (find-ns ns-sym)))))

(defn prepare!
  "Load every namespace in `prepare`, returning what was loaded.

   Checker EXTENSIONS register by side effect at load time — `chk/-invoke-special`
   defmethods, `t/ann` declarations — so a namespace whose types depend on one
   must be checked AFTER it is loaded. Measured with raster: the same namespace
   reports `Expected (t/Val 2048), Actual Long` with the extensions absent and
   checks clean with them present.

   The set of extension providers is OPEN, so this takes them as data rather than
   naming any one of them."
  [prepare]
  (doseq [ns-sym prepare] (require ns-sym))
  (vec prepare))

(defn check-violation
  "nil when the Typed Clojure checker accepts `ns-sym`; else a message.

   opts:
     :prepare  namespaces to load before checking (see `prepare!`)

   Three things are violations, not passes:

   - A namespace with no `^:typed.clojure` opt-in. `check-ns-info` answers
     `{:type-errors []}` for a namespace it SKIPPED, which is byte-identical to
     a clean check — so without this guard the whole rung-D facet is a green row
     asserting nothing, for every namespace that never opted in.
   - A checker that throws.
   - Reported type errors."
  ([ns-sym] (check-violation ns-sym nil))
  ([ns-sym {:keys [prepare]}]
   (prepare! prepare)
   (or (when-not (opted-in? ns-sym)
         (str ns-sym " was NOT CHECKED: it carries no ^:typed.clojure ns metadata, "
              "and check-ns-info reports an empty :type-errors for a namespace it "
              "skipped — indistinguishable from a clean check"))
       (let [errors (try (type-errors ns-sym)
                         (catch Throwable t
                           [{:message (str "checker threw: " (ex-message t))}]))]
         (when (seq errors)
           (str ns-sym " does not type-check: " (pr-str (mapv :message errors))))))))

(defmacro deftest-typed-check
  "Emit `name` — a deftest asserting the Typed Clojure checker accepts `ns-sym`.
   `opts` are `check-violation`'s; pass `{:prepare ['some.checker.extensions]}`
   when the namespace's types depend on an extension provider."
  ([name ns-sym] `(deftest-typed-check ~name ~ns-sym nil))
  ([name ns-sym opts]
   `(clojure.test/deftest ~name
      (let [v# (check-violation ~ns-sym ~opts)]
        (clojure.test/is (nil? v#) (str v#))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> type-errors [:=> [:cat :any] [:maybe [:sequential :any]]])

(m/=> opted-in? [:=> [:cat :any] :boolean])

(m/=> prepare! [:=> [:cat [:maybe [:sequential :any]]] [:vector :any]])

(m/=> check-violation [:function
                       [:=> [:cat :any] vocab/Violation]
                       [:=> [:cat :any [:maybe :map]] vocab/Violation]])
