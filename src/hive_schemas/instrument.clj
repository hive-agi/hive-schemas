(ns hive-schemas.instrument
  "Install malli FUNCTION contracts and check them generatively.

   `instrument!` ALTERS VAR ROOTS. After a `:reload-all` the wrappers are
   orphaned and a fresh JVM is required.

   Levers:
     contract-form   sym arg-schema [ret-schema] -> the (m/=> ..) form
     contracts       -> {qualified-sym schema}
     instrument!     install wrappers, whole registry or one namespace
     unstrument!     remove them
     check-all       -> nil | {qualified-sym explanation}
     check-violation -> nil | message
   One macro:
     deftest-instrumented-check  name opts -> a deftest over check-all"
  (:require [clojure.test]
            [hive-spi.schema.typed :as typed]
            [malli.core :as m]
            [malli.generator :as mg]
            [malli.instrument :as mi]
            [hive-schemas.vocab :as vocab]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn contract-form
  "The `(m/=> sym [:=> arg-schema ret-schema])` form registering `sym`'s function
   schema. `ret-schema` defaults to :hive/result."
  ([sym arg-schema] (typed/=>-form sym arg-schema))
  ([sym arg-schema ret-schema] (typed/=>-form sym arg-schema ret-schema)))

(defn contracts
  "{qualified-sym schema} for every function contract currently registered."
  []
  (into (sorted-map)
        (mapcat (fn [[ns-sym fns]]
                  (map (fn [[fn-sym data]]
                         [(symbol (str ns-sym) (str fn-sym)) (:schema data)])
                       fns)))
        (m/function-schemas)))

(defn- ns-options
  "Instrumentation options restricted to `ns-syms`, or nil for the whole
   registry."
  [ns-syms]
  (when (seq ns-syms)
    {:filters [(apply mi/-filter-ns ns-syms)]}))

(defn instrument!
  "Wrap every contracted var so calls are validated. Returns the number of vars
   instrumented. Restrict with `:ns` (a symbol or seq of symbols).

   Mutates var roots — see the namespace docstring."
  ([] (instrument! {}))
  ([{:keys [ns] :as options}]
   (let [ns-syms (cond (nil? ns) nil (symbol? ns) [ns] :else (vec ns))]
     (mi/instrument! (merge (dissoc options :ns) (ns-options ns-syms))))))

(defn unstrument!
  "Restore the original fns behind every contracted var."
  ([] (unstrument! {}))
  ([{:keys [ns] :as options}]
   (let [ns-syms (cond (nil? ns) nil (symbol? ns) [ns] :else (vec ns))]
     (mi/unstrument! (merge (dissoc options :ns) (ns-options ns-syms))))))

(defn check-all
  "malli's generative check of every contracted var: nil when all satisfy their
   `:=>` schema, else `{qualified-sym explanation}`, sorted.

   Arguments are generated, so the result is SAMPLED."
  ([] (check-all {}))
  ([{:keys [ns] :as options}]
   (let [ns-syms (cond (nil? ns) nil (symbol? ns) [ns] :else (vec ns))
         result  (mi/check (merge {:gen mg/generate}
                                  (dissoc options :ns)
                                  (ns-options ns-syms)))]
     (when (seq result)
       (into (sorted-map) result)))))

(defn check-violation
  "nil when every contracted var satisfies its schema on generated arguments;
   else a message naming the offending vars and the shrunk failing input."
  ([] (check-violation {}))
  ([options]
   (when-let [failures (check-all options)]
     (let [[sym explanation] (first failures)
           chk (-> explanation :errors first :check)]
       (str "contract check failed (sampled, not proven) for "
            (pr-str (vec (keys failures)))
            " — " sym " " (pr-str (:smallest chk))
            " -> " (pr-str (get chk :malli.core/result)))))))

(defmacro deftest-instrumented-check
  "Emit `name` — a test asserting every contracted var in scope satisfies its
   `m/=>` schema on generated arguments.

   opts: `:ns` restricts the check to one namespace or a seq of them."
  [name opts]
  `(clojure.test/deftest ~name
     (let [v# (check-violation ~opts)]
       (clojure.test/is (nil? v#) (str v#)))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> contract-form
      [:function
       [:=> [:cat vocab/SubjectRef vocab/SchemaRef] [:sequential :any]]
       [:=> [:cat vocab/SubjectRef vocab/SchemaRef vocab/SchemaRef] [:sequential :any]]])

(m/=> contracts [:=> [:cat] [:map-of :symbol :any]])

(m/=> instrument!
      [:function
       [:=> [:cat] [:sequential :any]]
       [:=> [:cat [:maybe vocab/Opts]] [:sequential :any]]])

(m/=> unstrument!
      [:function
       [:=> [:cat] [:sequential :any]]
       [:=> [:cat [:maybe vocab/Opts]] [:sequential :any]]])
