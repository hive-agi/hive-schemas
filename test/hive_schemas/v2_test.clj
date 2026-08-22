(ns hive-schemas.v2-test
  "Covers the plan/emit/coverage/wire/instrument/strength/stub/port/evolution
   layers, and pins the facet table against the macro and the clj-kondo hook."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.coverage :as cov]
            [hive-schemas.emit :as emit]
            [hive-schemas.evolution :as evo]
            [hive-schemas.plan :as plan]
            [hive-schemas.port :as port]
            [hive-schemas.strength :as strength]
            [hive-schemas.stub :as stub]
            [hive-schemas.test :as hst]
            [hive-schemas.wire :as wire]
            [hive-spi.schema.registry :as reg]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(reg/register! ::in  [:map [:x [:int {:min -1000 :max 1000}]]])
(reg/register! ::out [:map [:doubled :int]])

(defn dbl [{:keys [x]}] {:doubled (* 2 x)})
(defn dbl-rel [in out] (= (:doubled out) (* 2 (:x in))))

;; =============================================================================
;; plan
;; =============================================================================

(deftest opts->facets-reads-truthiness-not-presence
  (testing "a false opt selects no facet"
    (is (= [:conformance :mutants-present :mutations]
           (plan/opts->facets {:contract false :golden-path nil :rel nil}))))
  (testing "every selector, in emission order"
    (is (= [:conformance :relation :idempotent :contract
            :mutants-present :mutations :golden :input-strength]
           (plan/opts->facets {:rel 'r :idempotent? true :contract true
                               :golden-path "g.edn" :strict-in true}))))
  (testing "conformance is unconditional and the order follows facet-order"
    (doseq [opts [{} {:mutation false} {:rel 'r} {:contract true :strict-in true}]]
      (let [facets (plan/opts->facets opts)]
        (is (= :conformance (first facets)))
        (is (apply distinct? facets))
        (is (= facets (filterv (set facets) plan/facet-order)))))))

(deftest plan-round-trips-through-its-options
  (let [opts {:in ::in :out ::out :rel 'hive-schemas.v2-test/dbl-rel
              :contract true :num-tests 7}
        p    (plan/plan 'dbl-tests 'hive-schemas.v2-test/dbl opts)]
    (is (plan/validate p) (pr-str (plan/explain p)))
    (is (= :declared (:plan/provenance p)))
    (is (= opts (plan/plan->opts p)))
    (is (= 'hive-schemas.v2-test/dbl (:plan/subject p)))
    (is (= ['dbl-tests-conformance 'dbl-tests-relation 'dbl-tests-contract
            'dbl-tests-mutants-present 'dbl-tests-mutations]
           (plan/facet-vars p)))))

(deftest plan-records-provenance
  (is (= :inferred (:plan/provenance
                    (plan/plan 'p 'a/b {:in ::in :out ::out :provenance :inferred}))))
  (is (not (contains? (:plan/opts (plan/plan 'p 'a/b {:in ::in :out ::out :provenance :compiled}))
                      :provenance))))

(deftest subject-symbol-accepts-every-spelling
  (is (= 'a/b (plan/subject-symbol 'a/b)))
  (is (= 'a/b (plan/subject-symbol '(var a/b))))
  (is (= 'hive-schemas.v2-test/dbl (plan/subject-symbol #'dbl)))
  (is (thrown? clojure.lang.ExceptionInfo (plan/subject-symbol 42))))

;; =============================================================================
;; The drift gate: facet table <-> macro emission <-> clj-kondo hook
;; =============================================================================

(defn- emitted-facet-vars
  "Facet var names `deftrifecta-from-schema` actually emits for `opts`."
  [base opts]
  (let [form (macroexpand
              `(hst/deftrifecta-from-schema ~base hive-schemas.v2-test/dbl ~opts))]
    (into []
          (comp (filter seq?)
                (keep second)
                (filter symbol?)
                (filter #(str/starts-with? (str %) (str base)))
                (distinct))
          (tree-seq seq? seq form))))

(deftest facet-table-matches-what-the-macro-emits
  (doseq [opts [{:in ::in :out ::out}
                {:in ::in :out ::out :rel 'hive-schemas.v2-test/dbl-rel}
                {:in ::in :out ::out :contract true :mutation false}
                {:in ::in :out ::out :strict-in true :mutation false}
                {:in ::in :out ::out :golden-path "g.edn" :mutation false}
                {:in ::in :out ::out :rel 'hive-schemas.v2-test/dbl-rel
                 :contract true :golden-path "g.edn" :strict-in true}]]
    (is (= (plan/facet-vars (plan/plan 'drift 'hive-schemas.v2-test/dbl opts))
           (emitted-facet-vars 'drift opts))
        (str "plan/facet-vars disagrees with the macro for " (pr-str opts)))))

(deftest clj-kondo-hook-knows-every-facet-suffix
  (let [hook (slurp (io/resource "clj-kondo.exports/io.github.hive-agi/hive-schemas/hive_schemas/hooks/test.clj"))]
    (doseq [[facet suffix] plan/facet-suffix]
      (is (str/includes? hook (pr-str suffix))
          (str "the clj-kondo hook does not register the " facet
               " facet (" suffix "); a consumer would get an unresolved-var")))))

;; =============================================================================
;; emit
;; =============================================================================

(deftest render-ns-is-deterministic-and-readable
  (let [p    (plan/plan 'dbl-tests 'hive-schemas.v2-test/dbl
                        {:in ::in :out ::out :rel 'hive-schemas.v2-test/dbl-rel})
        spec {:ns 'gen.dbl-test :doc "Generated." :plans [p]}
        src  (emit/render-ns spec)]
    (is (= src (emit/render-ns spec)) "same spec, same bytes")
    (is (str/includes? src "hive-schemas.test/deftriad-from-schema"))
    (is (str/includes? src "GENERATED by hive-schemas.emit"))
    (testing "the rendered source reads back as forms"
      (let [forms (read-string (str "[" src "]"))]
        (is (= 'ns (ffirst forms)))
        (is (= 2 (count forms)))))))

(deftest preamble-forms-land-between-the-ns-form-and-the-plans
  (let [p       (plan/plan 'dbl-tests 'hive-schemas.v2-test/dbl {:in ::in :out ::out})
        setup   '(install! the-spec)
        rel-def '(def a-relation (relation-predicate the-spec :a/law))
        spec    {:ns 'gen.dbl-test :plans [p] :preamble [setup rel-def]}
        src     (emit/render-ns spec)
        forms   (read-string (str "[" src "]"))]
    (is (= src (emit/render-ns spec)) "a preamble stays deterministic")
    (is (= 'ns (ffirst forms)) "the ns form still comes first")
    (is (= [setup rel-def] (vec (rest (butlast forms))))
        "preamble forms render in order, right after the ns form")
    (is (= 'hive-schemas.test/deftriad-from-schema (first (last forms)))
        "the plans still come last")
    (testing "omitting :preamble renders exactly what it rendered before"
      (is (= (emit/render-ns {:ns 'gen.dbl-test :plans [p]})
             (emit/render-ns {:ns 'gen.dbl-test :plans [p] :preamble nil}))))))

(deftest emit-refuses-what-cannot-be-rendered
  (testing "a compiled :rel is not source"
    (let [p (plan/plan 'bad 'a/b {:in ::in :out ::out :rel (fn [_ _] true)})]
      (is (= :emit/unprintable
             (try (emit/plan->form p) nil
                  (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))
  (testing "a non-plan is refused before rendering"
    (is (= :emit/invalid-plan
           (try (emit/plan->form {:not :a-plan}) nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(deftest ns->path-maps-munged-names
  (is (= "gen/dbl_test.cljc" (emit/ns->path 'gen.dbl-test "cljc")))
  (is (= "a/b/c.clj" (emit/ns->path 'a.b.c))))

(deftest spit-ns-refuses-to-overwrite
  (let [root (str (java.nio.file.Files/createTempDirectory
                   "hive-schemas-emit" (into-array java.nio.file.attribute.FileAttribute [])))
        p    (plan/plan 'dbl-tests 'hive-schemas.v2-test/dbl {:in ::in :out ::out})
        spec {:ns 'gen.emitted-test :plans [p]}
        r1   (emit/spit-ns! root spec)]
    (is (:ok r1))
    (is (.exists (io/file (:ok r1))))
    (is (= :emit/exists (:error (emit/spit-ns! root spec))))
    (is (:ok (emit/spit-ns! root (assoc spec :overwrite? true))))))

;; =============================================================================
;; strength
;; =============================================================================

(deftest strength-scores-are-deterministic-and-ordered
  (is (= (strength/schema-strength ::in) (strength/schema-strength ::in)))
  (is (< (:rejection-rate (strength/schema-strength :any))
         (:rejection-rate (strength/schema-strength :int))
         (:rejection-rate (strength/schema-strength ::in)))))

(deftest vacuity-and-degeneracy-are-detected
  (is (:degenerate? (strength/schema-strength :any)))
  (is (not (:degenerate? (strength/schema-strength :int))))
  (is (some? (strength/input-vacuity :any)))
  (is (nil? (strength/input-vacuity ::in)))
  (testing "the malli -> typed projection reports its own collapse"
    (is (some? (strength/type-degeneracy [:fn even?])))
    (is (nil? (strength/type-degeneracy ::in)))))

;; =============================================================================
;; stub
;; =============================================================================

(deftest stubs-are-deterministic-and-conformant
  (is (= (stub/stub ::out) (stub/stub ::out)))
  (is (reg/validate ::out (stub/stub ::out)))
  (is (= 5 (count (stub/stub-seq ::in 5))))
  (is (every? #(reg/validate ::in %) (stub/stub-seq ::in 5))))

(deftest stub-fn-refuses-input-its-schema-rejects
  (let [f (stub/stub-fn ::in ::out)]
    (is (reg/validate ::out (f {:x 1})))
    (is (not= (f {:x 1}) (f {:x 1})) "varies with the call index")
    (is (= :stub/input-violation
           (try (f {:x "no"}) nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(deftest default-provider-fails-on-an-undeclared-method
  (let [p (stub/default-provider {'get-it ::out})]
    (is (reg/validate ::out (p 'get-it)))
    (is (= :stub/unknown-method
           (try (p 'nope) nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(deftest spy-observes-without-interfering
  (let [spied (stub/spy dbl {:in ::in :out ::out})]
    (is (= {:doubled 4} (spied {:x 2})) "returns exactly what the subject returns")
    (spied {:x 3})
    (spied {:x 2})
    (is (= 3 (count (stub/calls spied))))
    (is (empty? (stub/violations spied)))
    (testing "a broken subject is recorded, not thrown"
      (let [bad (stub/spy (fn [_] {:wrong true}) {:in ::in :out ::out})]
        (is (= {:wrong true} (bad {:x 1})))
        (is (= 1 (count (stub/violations bad))))
        (is (false? (:out-ok? (first (stub/violations bad)))))))
    (testing "an unsupplied schema records nil, not true"
      (let [unobserved (stub/spy dbl {:in ::in})]
        (unobserved {:x 1})
        (is (nil? (:out-ok? (first (stub/calls unobserved)))))))
    (testing "traces become golden cases, deduplicated by input"
      (is (= {:case-0 {:in {:x 2} :out {:doubled 4}}
              :case-1 {:in {:x 3} :out {:doubled 6}}}
             (stub/trace->cases (stub/calls spied)))))))

;; =============================================================================
;; wire
;; =============================================================================

(deftest wire-boundary-levers
  (is (nil? (wire/coercion-identity-violation ::in)))
  (is (nil? (wire/wire-roundtrip-violation ::in)))
  (is (nil? (wire/explain-total-violation ::in)))
  (is (nil? (wire/json-schema-violation ::in)))
  (testing "a schema that constrains nothing has an empty JSON Schema"
    (is (some? (wire/json-schema-violation :any)))))

(wire/deftrifecta-wire wire-in ::in {:roundtrip true})

;; =============================================================================
;; coverage
;; =============================================================================

(defn- write-source!
  [dir file-name content]
  (let [f (io/file dir file-name)]
    (io/make-parents f)
    (spit f content)
    f))

(defn- temp-dir []
  (str (java.nio.file.Files/createTempDirectory
        "hive-schemas-coverage" (into-array java.nio.file.attribute.FileAttribute []))))

(deftest coverage-universe-comes-from-the-files
  (let [dir (temp-dir)]
    (write-source! dir "probe/one.clj"
                   "(ns probe.one)\n(defn alpha [x] x)\n(defn- hidden [x] x)\n(def beta 1)\n")
    (write-source! dir "probe/two.cljc"
                   "(ns probe.two)\n(defn gamma [x] #?(:clj x :cljs x))\n")
    (let [r (cov/coverage {:paths [dir] :require-namespaces? false})]
      (testing "public defns across clj and cljc, private and def excluded"
        (is (= #{'probe.one/alpha 'probe.two/gamma} (set (:universe r)))))
      (testing "private defns are opt-in"
        (is (contains? (set (:universe (cov/coverage {:paths [dir]
                                                      :include-private? true
                                                      :require-namespaces? false})))
                       'probe.one/hidden)))
      (is (empty? (:unreadable r))))))

(deftest coverage-gates-are-independently-falsifiable
  (let [dir (temp-dir)]
    (write-source! dir "probe/three.clj" "(ns probe.three)\n(defn delta [x] x)\n")
    (let [base {:paths [dir] :require-namespaces? false}]
      (testing "an empty universe fails rather than passing vacuously"
        (let [failures (cov/coverage-failures (cov/coverage (assoc base :paths [(temp-dir)])))]
          (is (some #(str/includes? % "vacuous") failures))))
      (testing "an uncontracted function is reported"
        (is (some #(str/includes? % "uncontracted")
                  (cov/coverage-failures (cov/coverage base)))))
      (testing "an exemption clears it"
        (is (empty? (cov/coverage-failures
                     (cov/coverage (assoc base :exempt {'probe.three/delta "probe fixture"}))))))
      (testing "an exemption without a reason is itself a failure"
        (is (some #(str/includes? % "without a reason")
                  (cov/coverage-failures
                   (cov/coverage (assoc base :exempt {'probe.three/delta "  "}))))))
      (testing "an exemption for a function that no longer exists is stale"
        (is (some #(str/includes? % "stale")
                  (cov/coverage-failures
                   (cov/coverage (assoc base :exempt {'probe.three/delta "ok"
                                                      'probe.three/gone "reason"})))))))))

(deftest coverage-records-what-it-could-not-read
  (let [dir (temp-dir)]
    (write-source! dir "probe/broken.clj" "(ns probe.broken)\n(defn oops [x] #_unclosed (")
    (let [r (cov/coverage {:paths [dir] :require-namespaces? false})]
      (is (= 1 (count (:unreadable r))))
      (is (some #(str/includes? % "unreadable") (cov/coverage-failures r))))))

;; =============================================================================
;; evolution
;; =============================================================================

(deftest compatibility-is-variance-correct
  (let [narrow [:map [:a :int]]
        wide   [:map [:a :int] [:b {:optional true} :string]]
        wider  [:map [:a :int] [:b :string]]]
    (testing "an input schema may widen"
      (is (nil? (evo/compat-violation narrow wide {:variance :input})))
      (is (some? (evo/compat-violation narrow wider {:variance :input}))))
    (testing "an output schema may narrow"
      (is (nil? (evo/compat-violation narrow wider {:variance :output})))
      (is (some? (evo/compat-violation wider narrow {:variance :output}))))
    (testing "the message names its own rung"
      (is (str/includes? (evo/compat-violation narrow wider {:variance :input})
                         "SAMPLED")))))

(deftest breaking-changes-reports-removals-and-narrowings
  (is (= {:gone "schema removed from the registry"}
         (evo/breaking-changes {:gone {:form :int}} {} {})))
  (is (empty? (evo/breaking-changes {:k {:form [:map [:a :int]]}}
                                    {:k {:form [:map [:a :int]]}} {})))
  (is (contains? (evo/breaking-changes {:k {:form [:map [:a :int]]}}
                                       {:k {:form [:map [:a :int] [:b :string]]}} {})
                 :k)))

(deftest registry-snapshot-covers-the-registered-keys
  (let [snap (evo/registry-snapshot [::in ::out])]
    (is (= [::in ::out] (keys snap)))
    (is (every? #(contains? % :form) (vals snap)))
    (is (= snap (evo/registry-snapshot [::in ::out])) "deterministic")))

;; =============================================================================
;; port
;; =============================================================================

(defprotocol IDoubler
  (double-it [this n]))

(defrecord Faithful []
  IDoubler
  (double-it [_ n] {:doubled (* 2 n)}))

(defrecord Broken []
  IDoubler
  (double-it [_ _] "not a map"))

(def doubler-spec
  {:port/methods {'double-it {:in [:cat [:int {:min -100 :max 100}]]
                              :out ::out
                              :rel (fn [[n] out] (= (:doubled out) (* 2 n)))}}})

(deftest one-law-suite-separates-adapters
  (is (empty? (port/port-violations doubler-spec {'double-it double-it} (->Faithful))))
  (is (contains? (port/port-violations doubler-spec {'double-it double-it} (->Broken))
                 'double-it))
  (testing "a declared method with no implementation is a violation"
    (is (contains? (port/port-violations doubler-spec {} (->Faithful)) 'double-it)))
  (testing "a spec whose keys are not symbols is refused, not mis-sorted"
    (is (= :port/invalid-spec
           (try (port/port-violations {:port/methods {double-it {:in [:cat :int] :out ::out}}}
                                      {} (->Faithful))
                nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))

(port/deftest-port-contract faithful-contract
  {:port/methods {double-it {:in [:cat [:int {:min -100 :max 100}]] :out ::out}}}
  (->Faithful))

;; =============================================================================
;; The bridge still synthesizes, through the plan
;; =============================================================================

(def dbl-plan
  (plan/plan 'planned-dbl 'hive-schemas.v2-test/dbl
             {:in ::in :out ::out :rel 'hive-schemas.v2-test/dbl-rel
              :contract true :strict-in true :num-tests 30}))

(hst/deftriad-from-plan dbl-plan)
