(ns hive-schemas.wire
  "Boundary levers over a schema's `compile-op` bundle, in the nil/violation-
   message idiom.

   Levers:
     coercion-identity-violation  ?s -> nil | msg
     wire-roundtrip-violation     ?s -> nil | msg
     explain-total-violation      ?s -> nil | msg
     json-schema-violation        ?s -> nil | msg
   One macro:
     deftrifecta-wire  name ?s opts -> the boundary facets as test vars"
  (:require [clojure.test]
            [hive-schemas.strength :as strength]
            [hive-spi.schema.derive :as derive]
            [hive-spi.schema.gen :as sgen]
            [hive-spi.schema.registry :as reg]
            [malli.core :as m]
            [malli.transform :as mt]
            [hive-schemas.vocab :as vocab])
  #?(:cljs (:require-macros [hive-schemas.wire])))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def wire-transformer
  "The transformer a hive op boundary encodes through: malli's JSON transformer."
  (mt/transformer mt/json-transformer))

(defn- cases
  "A reproducible sample of `n` values conforming to `?schema`."
  [?schema seed n]
  (vals (sgen/seeded-cases ?schema seed n)))

(defn coercion-identity-violation
  "nil when coercing an ALREADY-VALID value leaves it unchanged; else a message
   naming the first value coercion altered or threw on."
  ([?schema] (coercion-identity-violation ?schema {}))
  ([?schema {:keys [seed n] :or {seed 0 n 16}}]
   (let [coerce (:coerce (derive/compile-op ?schema))]
     (some (fn [v]
             (let [c (try (coerce v) (catch #?(:clj Exception :cljs :default) e e))]
               (cond
                 (instance? #?(:clj Exception :cljs js/Error) c)
                 (str "coercion THREW on a conforming value " (pr-str v) ": " (ex-message c))

                 (not= c v)
                 (str "coercion is not identity on the conforming value "
                      (pr-str v) " -> " (pr-str c)))))
           (cases ?schema seed n)))))

(defn wire-roundtrip-violation
  "nil when every generated value survives an encode/decode round trip through
   `wire-transformer`; else a message naming the first value that did not.

   A lossy wire schema fails this; the facet is opt-in."
  ([?schema] (wire-roundtrip-violation ?schema {}))
  ([?schema {:keys [seed n] :or {seed 0 n 16}}]
   (let [s      (reg/schema ?schema)
         encode (m/encoder s wire-transformer)
         decode (m/decoder s wire-transformer)]
     (some (fn [v]
             (let [r (try (decode (encode v))
                          (catch #?(:clj Exception :cljs :default) e e))]
               (cond
                 (instance? #?(:clj Exception :cljs js/Error) r)
                 (str "wire round-trip THREW on " (pr-str v) ": " (ex-message r))

                 (not= r v)
                 (str "wire round-trip is lossy: " (pr-str v) " -> " (pr-str r)))))
           (cases ?schema seed n)))))

(defn explain-total-violation
  "nil when `explain` returns a value for every input on `values` (default the
   strength universe) rather than throwing; else a message naming the input that
   threw."
  ([?schema] (explain-total-violation ?schema strength/universe))
  ([?schema values]
   (some (fn [v]
           (try (reg/explain ?schema v) nil
                (catch #?(:clj Exception :cljs :default) e
                  (str "explain THREW on " (pr-str v) ": " (ex-message e)))))
         values)))

(defn json-schema-violation
  "nil when `?schema` projects to a non-empty JSON Schema map; else a message."
  [?schema]
  (let [js (try (derive/input-schema ?schema)
                (catch #?(:clj Exception :cljs :default) e e))]
    (cond
      (instance? #?(:clj Exception :cljs js/Error) js)
      (str "JSON Schema projection THREW: " (ex-message js))

      (not (map? js))
      (str "JSON Schema projection is not a map: " (pr-str js))

      (empty? js)
      "JSON Schema projection is empty — the boundary would accept anything")))

(defmacro deftrifecta-wire
  "Synthesize the rung-0 boundary facets for `?schema`.

   opts:
     :roundtrip    also assert encode/decode is lossless   [optional]
     :json-schema  also assert the MCP projection survives  (default true)
     :seed/:n      sampling of the generated values         (0 / 16)

   Facets emitted:
     <name>-coercion-identity  coercion is a no-op on conforming values
     <name>-explain-total      explain never throws
     <name>-wire-roundtrip     decode . encode = identity   [when :roundtrip]
     <name>-json-schema        the JSON Schema projection is a non-empty map"
  [name ?schema {:keys [roundtrip json-schema seed n]
                 :or   {json-schema true seed 0 n 16}}]
  (let [is-sym      (if (:ns &env) 'cljs.test/is 'clojure.test/is)
        deftest-sym (if (:ns &env) 'cljs.test/deftest 'clojure.test/deftest)
        opts        {:seed seed :n n}]
    `(do
       (~deftest-sym ~(symbol (str name "-coercion-identity"))
         (let [v# (coercion-identity-violation ~?schema ~opts)]
           (~is-sym (nil? v#) (str v#))))
       (~deftest-sym ~(symbol (str name "-explain-total"))
         (let [v# (explain-total-violation ~?schema)]
           (~is-sym (nil? v#) (str v#))))
       ~@(when roundtrip
           [`(~deftest-sym ~(symbol (str name "-wire-roundtrip"))
               (let [v# (wire-roundtrip-violation ~?schema ~opts)]
                 (~is-sym (nil? v#) (str v#))))])
       ~@(when json-schema
           [`(~deftest-sym ~(symbol (str name "-json-schema"))
               (let [v# (json-schema-violation ~?schema)]
                 (~is-sym (nil? v#) (str v#))))]))))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> coercion-identity-violation
      [:function
       [:=> [:cat vocab/SchemaRef] vocab/Violation]
       [:=> [:cat vocab/SchemaRef [:maybe vocab/Opts]] vocab/Violation]])

(m/=> wire-roundtrip-violation
      [:function
       [:=> [:cat vocab/SchemaRef] vocab/Violation]
       [:=> [:cat vocab/SchemaRef [:maybe vocab/Opts]] vocab/Violation]])

(m/=> explain-total-violation
      [:function
       [:=> [:cat vocab/SchemaRef] vocab/Violation]
       [:=> [:cat vocab/SchemaRef [:sequential :any]] vocab/Violation]])

(m/=> json-schema-violation [:=> [:cat vocab/SchemaRef] vocab/Violation])
