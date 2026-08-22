(ns hive-schemas.port
  "One generated law suite that every adapter of a port must pass.

   The adapter is INJECTED; this namespace names no concretion. The suite calls
   the real methods with generated arguments, so point it at a sandboxed instance
   and use `:pre` to keep arguments inside the states a method admits.

   Registered schemas:
     :hive.schemas/method-spec  one method's contract
     :hive.schemas/port-spec    a port's method map

   Levers:
     method-violations  f instance method-spec opts -> [message ...]
     port-violations    spec {method fn} instance opts -> {method [message ...]}
   One macro:
     deftest-port-contract  name spec instance opts -> the per-method facets"
  (:require [clojure.test]
            [hive-schemas.stub :as stub]
            [hive-schemas.subject :as subject]
            [hive-spi.schema.registry :as reg]
            [hive-schemas.vocab :as vocab]
            [malli.core :as m])
  #?(:cljs (:require-macros [hive-schemas.port])))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def MethodSpec
  "One port method's contract. `:in` is the argument list AFTER the instance."
  [:map {:closed false}
   [:in :any]
   [:out :any]
   [:rel {:optional true} :any]
   [:pre {:optional true} :any]])

(def PortSpec
  "A port: its methods by symbol."
  [:map {:closed false}
   [:port/methods [:map-of :symbol MethodSpec]]])

(reg/register-all! {:hive.schemas/method-spec MethodSpec
                    :hive.schemas/port-spec   PortSpec})

(def ^:private default-cases 8)

(defn- thrown
  "The throwable `body-fn` raised, or nil."
  [body-fn]
  (try (body-fn) nil
       (catch #?(:clj Throwable :cljs :default) t t)))

(defn method-violations
  "Messages for every generated call to `f` on `instance` that broke the method's
   contract.

   `f` takes the instance first. Arguments come from `:in` as a reproducible
   seeded sample. A method that THROWS is a violation."
  ([f instance method-spec] (method-violations f instance method-spec {}))
  ([f instance {:keys [in out rel pre]} {:keys [seed n] :or {seed 0 n default-cases}}]
   (let [arglist? (subject/arglist-schema? in)]
     (into []
           (keep (fn [generated]
                   (let [argv (if arglist? (vec generated) [generated])]
                     (when (or (nil? pre) (pre argv))
                       (let [result (volatile! nil)
                             t      (thrown #(vreset! result (apply f instance argv)))
                             ret    @result]
                         (cond
                           t
                           (str "method THREW on args " (pr-str argv) ": " (ex-message t))

                           (not (reg/validate out ret))
                           (str "output violates :out for args " (pr-str argv)
                                " -> " (pr-str ret))

                           (and rel (not (rel argv ret)))
                           (str "relation violated for args " (pr-str argv)
                                " -> " (pr-str ret))))))))
           (stub/stub-seq in n {:seed seed})))))

(defn port-violations
  "`{method [message ...]}` for every method of `spec` that `instance` breaks.

   `method-fns` maps each declared method symbol to its fn; a declared method
   with no fn is itself a violation. Method keys must be QUOTED symbols — a spec
   that does not conform to :hive.schemas/port-spec is refused with
   `:port/invalid-spec`."
  ([spec method-fns instance] (port-violations spec method-fns instance {}))
  ([spec method-fns instance opts]
   (when-let [problem (reg/explain :hive.schemas/port-spec spec)]
     (throw (ex-info "Port spec does not conform to :hive.schemas/port-spec"
                     {:error :port/invalid-spec :explanation (:errors problem)})))
   (into (sorted-map)
         (keep (fn [[method method-spec]]
                 (if-let [f (get method-fns method)]
                   (when-let [msgs (seq (method-violations f instance method-spec opts))]
                     [method (vec msgs)])
                   [method [(str "no implementation supplied for declared method " method)]])))
         (:port/methods spec))))

(defmacro deftest-port-contract
  "Emit the law suite `spec` declares, run against the INJECTED `instance`.

   `spec` must be a LITERAL map: the method symbols are read from the form while
   emitting, so each becomes its own test var. A `spec` that is not a literal
   declares no methods and the non-vacuity facet fails.

   opts: `:seed` / `:n` control the generated argument sample.

   Facets emitted:
     <name>-methods-present   FAILS LOUD when the spec declares no methods
     <name>-<method>          every generated call satisfies :out and :rel"
  [name spec instance & {:keys [seed n] :or {seed 0 n default-cases}}]
  (let [is-sym      (if (:ns &env) 'cljs.test/is 'clojure.test/is)
        deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)
        methods     (when (map? spec) (:port/methods spec))
        opts        {:seed seed :n n}]
    `(do
       (~deftest-sym ~(symbol (str name "-methods-present"))
         (~is-sym ~(pos? (count methods))
                  "port spec declares no methods — the contract suite would be vacuous"))
       ~@(for [[method method-spec] methods]
           `(~deftest-sym ~(symbol (str name "-" (clojure.core/name method)))
              (let [msgs# (method-violations ~method ~instance ~method-spec ~opts)]
                (~is-sym (empty? msgs#)
                         (str '~method " violated its port contract: "
                              (pr-str msgs#)))))))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> method-violations
      [:function
       [:=> [:cat :any :any [:map]] vocab/Violations]
       [:=> [:cat :any :any [:map] [:maybe vocab/Opts]] vocab/Violations]])

(m/=> port-violations
      [:function
       [:=> [:cat :any [:map] :any] [:map-of :symbol vocab/Violations]]
       [:=> [:cat :any [:map] :any [:maybe vocab/Opts]] [:map-of :symbol vocab/Violations]]])
