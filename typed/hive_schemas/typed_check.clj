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

(defn check-violation
  "nil when the Typed Clojure checker accepts `ns-sym`; else a message naming its
   type errors. A checker that THROWS is reported as a violation, never as a
   pass."
  [ns-sym]
  (let [errors (try (type-errors ns-sym)
                    (catch Throwable t
                      [{:message (str "checker threw: " (ex-message t))}]))]
    (when (seq errors)
      (str ns-sym " does not type-check: " (pr-str (mapv :message errors))))))

(defmacro deftest-typed-check
  "Emit `name` — a deftest asserting the Typed Clojure checker accepts `ns-sym`."
  [name ns-sym]
  `(clojure.test/deftest ~name
     (let [v# (check-violation ~ns-sym)]
       (clojure.test/is (nil? v#) (str v#)))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> type-errors [:=> [:cat :any] [:maybe [:sequential :any]]])

(m/=> check-violation [:=> [:cat :any] vocab/Violation])
