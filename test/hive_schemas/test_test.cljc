(ns hive-schemas.test-test
  "Proves the schema -> free-tests bridge: runtime levers (unit) + the
   synthesized property/mutation facets running as real tests."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-schemas.test :as hst]
            [hive-spi.schema.registry :as reg]
            [hive-test.stateful :as sf]
            [hive-test.trifecta :as tri]
            [malli.core :as m]))

;; --- a schematized subject (bounds keep the product within long range;
;;     malli bounds flow straight into the synthesized input generator) ---
(reg/register! ::in  [:map
                      [:x [:int {:min -10000 :max 10000}]]
                      [:y [:int {:min -10000 :max 10000}]]])
(reg/register! ::out [:map [:sum :int] [:product :int]])

(defn calc [{:keys [x y]}] {:sum (+ x y) :product (* x y)})
(defn calc-bug [{:keys [x y]}] {:sum (- x y) :product (* x y)})  ; wrong :sum

(defn calc-rel [in out]
  (and (= (:sum out)     (+ (:x in) (:y in)))
       (= (:product out) (* (:x in) (:y in)))))

;; --- runtime levers ---

(deftest required-entries-test
  (is (= [:sum :product] (mapv first (hst/required-entries ::out))))
  (is (nil? (hst/required-entries [:int]))))

(deftest output-oracle-test
  (let [ok? (hst/output-oracle ::out)]
    (is (ok? {:sum 7 :product 12}))
    (is (not (ok? {:sum 7})))                 ; missing required key
    (is (not (ok? {:sum "7" :product 12})))))  ; wrong type

(deftest schema-mutants-test
  (let [muts (hst/schema-mutants calc ::out)
        ok?  (hst/output-oracle ::out)]
    (is (= ["drop-key-:sum" "assoc-const-:sum" "drop-key-:product" "assoc-const-:product"]
           (mapv first muts)))
    (is (ok? (calc {:x 3 :y 4})))                                 ; good conforms
    (is (every? (fn [[_ f]] (not (ok? (f {:x 3 :y 4})))) muts))))  ; every mutant killed

;; --- scalar outputs: required-entries is map-shaped, so an :enum or a bounded
;;     number synthesized ZERO mutants and the non-vacuity guard fired ---

(reg/register! ::zone     [:enum :domestic :mercosur :international])
(reg/register! ::priority [:int {:min 1 :max 9}])

(defn zone-of     [n] (nth [:domestic :mercosur :international] (mod n 3)))
(defn priority-of [n] (inc (mod n 9)))

(deftest scalar-mutants-enum-test
  (let [muts (hst/schema-mutants zone-of ::zone)
        ok?  (hst/output-oracle ::zone)]
    (is (seq muts) "an :enum output must synthesize at least one mutant")
    (is (ok? (zone-of 2)))
    (is (every? (fn [[_ f]] (not (ok? (f 2)))) muts))))

(deftest scalar-mutants-bounds-test
  (let [muts (hst/schema-mutants priority-of ::priority)
        ok?  (hst/output-oracle ::priority)]
    (testing "both bounds become mutants"
      (is (contains? (set (mapv first muts)) "const-0"))
      (is (contains? (set (mapv first muts)) "const-10")))
    (is (ok? (priority-of 4)))
    (is (every? (fn [[_ f]] (not (ok? (f 4)))) muts))))

(deftest scalar-mutants-soundness-test
  (testing "a schema that rejects nothing yields no mutants rather than a survivor"
    (is (empty? (hst/schema-mutants identity :any))))
  (testing "map outputs are untouched by the scalar branch"
    (is (= ["drop-key-:sum" "assoc-const-:sum" "drop-key-:product" "assoc-const-:product"]
           (mapv first (hst/schema-mutants calc ::out))))))

;; --- triad-in-one: malli facets + optional proof/model-check legs, one entry ---
;; No :prove here (JVM-safe under plain :test): the malli facets run off the
;; schema. :optional? true is what makes the model-check facet green without
;; recife on the classpath — this suite is asserting that the facet is EMITTED,
;; not that any model was checked.
(hst/deftriad-from-schema triad-calc #'calc
  {:in ::in :out ::out :rel calc-rel
   :model-check {:model-spec {:init-state {} :components #{}} :optional? true}})

(deftest deftriad-composes-the-ladder
  (testing "the malli conformance + relation facets are emitted as real test vars"
    (is (some? #'triad-calc-conformance))
    (is (some? #'triad-calc-relation)))
  (testing "the model-check facet is emitted, and an absent recife is a SKIP only
            because :optional? true asked for one"
    (is (some? #'triad-calc-model-check)))
  (testing "without :optional? an unreachable checker FAILS rather than passing —
            a green facet in a project with no model checker is false assurance"
    (let [expand (fn [opts] (pr-str (macroexpand-1
                                      `(hst/deftriad-from-schema mc# my.ns/f ~opts))))
          spec   {:model-spec {:init-state {} :components #{}}}]
      (is (re-find #"\(clojure\.test/is false" (expand {:in ::in :out ::out :model-check spec})))
      (is (re-find #"\(clojure\.test/is true"
                   (expand {:in ::in :out ::out
                            :model-check (assoc spec :optional? true)})))))
  (testing "a :prove opt emits an ansatz PROOF facet (fully-qualified,
            consumer-resolved) — checked by expansion, so no kernel is needed"
    (let [form (macroexpand-1
                '(hive-schemas.test/deftriad-from-schema tri-p my.ns/f
                   {:in ::in :out ::out
                    :prove {:params '[] :prop '(= x x) :tactics '[(rfl)]}}))]
      (is (some #(and (seq? %)
                      (= 'hive-schemas.proven/deftrifecta-proven (first %)))
                (tree-seq seq? seq form))
          "expansion carries the proven proof facet, decoupled from load"))))

(deftest seeded-cases-deterministic-test
  (is (= (hst/seeded-cases ::in 42 6) (hst/seeded-cases ::in 42 6)))
  (is (= 6 (count (hst/seeded-cases ::in 42 6)))))

(deftest seeded-cases-coheres-with-the-derivation-lever
  ;; mirror == lever, whenever the pinned hive-spi ships seeded-cases
  #?(:clj (if-let [lever (resolve 'hive-spi.schema.gen/seeded-cases)]
            (doseq [schema [:hive/result [:int] [:map [:q :string]]]]
              (is (= (hst/seeded-cases schema 42 8) (lever schema 42 8))
                  (str schema ": mirror drifted from hive-spi.schema.gen/seeded-cases")))
            (is true "pinned hive-spi predates seeded-cases — coherence deferred"))
     :cljs (is true "clj-only: resolve-based lever probe")))

;; --- the contract lever: mg/check catches a wrong :sum, passes a correct calc ---
(deftest contract-violation-test
  (is (nil?  (hst/contract-violation ::in ::out calc-rel calc)))
  (is (some? (hst/contract-violation ::in ::out calc-rel calc-bug)))
  ;; with no rel it degrades to output-conformance: a missing key is caught
  (is (some? (hst/contract-violation ::in ::out nil (fn [_] {:sum 1})))))

;; --- the CLASSIC hive-test paradigm consuming registry-sourced facets:
;;     :gen/:pred/:cases are lever calls in the literal spec — no hand-written
;;     generator, oracle, or case table; mutation teeth are golden-derived
;;     from the seeded schema cases.
(tri/deftrifecta calc-lever-facets
  hive-schemas.test-test/calc
  {:gen         (hst/input-gen ::in)
   :pred        (hst/output-oracle ::out)
   :num-tests   200
   :cases       (hst/seeded-cases ::in 0 6)
   :golden-path "test/hive_schemas/__golden__/calc-lever-facets.edn"
   :mutations   [["swaps :sum to subtraction" calc-bug]]})

;; --- the headline: property + mutation + CONTRACT tests synthesized from schemas ---
(hst/deftrifecta-from-schema calc-tests hive-schemas.test-test/calc
  {:in ::in :out ::out :rel calc-rel :contract true :num-tests 50 :n-cases 5
   :golden-path "test/hive_schemas/__golden__/calc-tests.edn"})

;; =============================================================================
;; resolve-map-schema reach: :maybe deref + :or/:multi intersection
;; =============================================================================

;; =============================================================================
;; Arglist :in — a MULTI-ARG subject schematized as [:cat A B]
;; =============================================================================

(defn scale
  "Multi-arg subject."
  [factor xs]
  {:factor factor
   :scaled (mapv #(* factor %) xs)})

(def ^:private scale-args
  [:cat [:int {:min -1000 :max 1000}] [:vector [:int {:min -1000 :max 1000}]]])

(def ^:private scale-out
  [:map {:closed true}
   [:factor :int]
   [:scaled [:vector :int]]])

(hst/deftrifecta-from-schema scale-tests hive-schemas.test-test/scale
  {:in        scale-args
   :out       scale-out
   :rel       (fn [[factor xs] out]
                (and (= factor (:factor out))
                     (= (mapv #(* factor %) xs) (:scaled out))))
   :contract  true
   :num-tests 50
   :n-cases   5})

(deftest schema-arity-dispatches-on-schema-type
  (testing ":cat / :catn are arglists; every other schema is one value"
    (is (= :arglist (hst/schema-arity [:cat :int :string])))
    (is (= :arglist (hst/schema-arity [:catn [:a :int] [:b :string]])))
    (is (= :value (hst/schema-arity [:map [:a :int]])))
    (is (= :value (hst/schema-arity [:vector :int])))
    (is (= :value (hst/schema-arity :int))))
  (testing "arglist-schema? agrees"
    (is (true? (hst/arglist-schema? [:cat :int])))
    (is (true? (hst/arglist-schema? [:catn [:a :int]])))
    (is (false? (hst/arglist-schema? [:map [:a :int]])))))

(deftest applier-applies-arglists-and-passes-values
  (testing "an arglist schema spreads the generated seq over the subject"
    (is (= 7 ((hst/applier [:cat :int :int]) + [3 4]))))
  (testing "any other schema hands the value over whole"
    (is (= {:a 1} ((hst/applier [:map [:a :int]]) identity {:a 1})))))

(deftest required-entries-unions-test
  ;; :maybe derefs to the inner map (card: explain -> [:maybe :map])
  (is (= [:a :b] (mapv first (hst/required-entries [:maybe [:map [:a :int] [:b :string]]]))))
  ;; :or over maps -> keys required in EVERY branch (intersection)
  (is (= [:a] (mapv first (hst/required-entries
                            [:or [:map [:a :int] [:b :int]] [:map [:a :string]]]))))
  ;; a key required in one branch but OPTIONAL in another is NOT sound -> excluded
  (is (empty? (hst/required-entries
                [:or [:map [:a :int]] [:map [:a {:optional true} :int] [:c :int]]])))
  ;; a non-map (permissive) branch -> no sound intersection -> nil
  (is (nil? (hst/required-entries [:or [:map [:a :int]] :string])))
  ;; :multi excludes the dispatch key, intersects the rest
  (is (= [:a] (mapv first (hst/required-entries
                            [:multi {:dispatch :type}
                             [:x [:map [:type :keyword] [:a :int]]]
                             [:y [:map [:type :keyword] [:a :string] [:b :int]]]]))))
  ;; a fn dispatch is not analyzable -> nil
  (is (nil? (hst/required-entries [:multi {:dispatch (fn [_] :x)} [:x [:map [:a :int]]]]))))

(deftest maybe-mutants-sound-test
  ;; card 57a9abaa: an [:maybe :map] output (nil | detail-map, e.g. malli explain)
  ;; used to yield ZERO mutants (vacuous facet); now it yields SOUND ones.
  (let [out  [:maybe [:map [:a :int] [:b :string]]]
        ok?  (hst/output-oracle out)
        orig (fn [_] {:a 1 :b "x"})
        muts (hst/schema-mutants orig out)]
    (is (pos? (count muts)))                              ; non-vacuous
    (is (ok? nil))                                        ; nil IS a valid output
    (is (ok? (orig :_)))                                  ; so is the full map
    (is (every? (fn [[_ f]] (not (ok? (f :_)))) muts))))  ; every mutant killed

;; --- an :or-output subject: both branches maps, so the mutation facet is
;;     non-vacuous and each intersection-key mutant is provably killed ---
(reg/register! ::tag-in  [:map [:n [:int {:min 0 :max 10000}]]])
(reg/register! ::tag-out
  [:or
   [:map [:kind [:= :even]] [:n :int] [:half :int]]
   [:map [:kind [:= :odd]]  [:n :int]]])

(defn classify [{:keys [n]}]
  (if (even? n)
    {:kind :even :n n :half (quot n 2)}
    {:kind :odd :n n}))

(hst/deftrifecta-from-schema classify-tests hive-schemas.test-test/classify
  {:in ::tag-in :out ::tag-out :num-tests 50 :n-cases 5})

;; =============================================================================
;; Stateful machine levers: malli drives the command alphabet + the state law
;; =============================================================================

(reg/register! ::bump-args [:int {:min 1 :max 2}])
(reg/register! ::counter   [:map [:n [:int {:min 0 :max 3}]]])

(deftest command-gen-test
  (let [args ((hst/command-gen ::bump-args {:seed 7 :n 4}) :irrelevant-model)]
    (is (= 4 (count args)))
    (is (every? #(<= 1 % 2) args))
    (is (= args ((hst/command-gen ::bump-args {:seed 7 :n 4}) :another-model))
        "deterministic and model-independent"))
  (is (some? (hst/command-gen ::bump-args {:as :gen}))
      ":as :gen yields a test.check generator (sampled, not enumerable)"))

(deftest malli-drives-a-stateful-machine
  ;; command args sampled from ::bump-args, state law pinned by ::counter —
  ;; the machine spec carries no hand-written generator or oracle
  (let [machine {:init       (fn [] {:n 0})
                 :commands   {:bump {:args (hst/command-gen ::bump-args {:seed 1 :n 4})
                                     :pre  (fn [m d] (<= (+ (:n m) d) 3))
                                     :next (fn [m d] (update m :n + d))}}
                 :invariants {:bounded (hst/model-step ::counter)}
                 :goals      {:maxed (fn [m] (= 3 (:n m)))}}
        r       (sf/check machine {:max-states 100 :max-depth 10})]
    (is (:ok? r) (sf/report-str r))))

(deftest model-step-catches-a-violation
  (let [machine {:init       (fn [] {:n 0})
                 :commands   {:over {:args (fn [_] [9])
                                     :next (fn [m d] (update m :n + d))}}
                 :invariants {:bounded (hst/model-step ::counter)}}
        r       (sf/check machine {})]
    (is (false? (:ok? r)))
    (is (seq (:invariant-violations r)))))
;; =============================================================================
;; A schema carrying its OWN :registry
;; =============================================================================
;;
;; A recursive schema is spelled `[:schema {:registry {::x ...}} ::x]`, so the
;; ref that closes the loop resolves only against the schema's own registry —
;; not the hive one, and not malli's default. Every lever here re-schemas what
;; it is handed, so if any of them re-forms through a foreign registry, `::node`
;; resolves to nil and the failure reads
;;
;;   No implementation of method: :-schema of protocol: #'malli.registry/Registry
;;   found for class: nil
;;
;; which names neither the schema nor the lever. Filed from hive-system 2026-08-21
;; as [HIVE-SCHEMAS-LOCAL-REGISTRY]; the suite had NO recursive schema at all, so
;; nothing here would have caught a regression either way. These pin it.

(def ^:private Node
  "Self-referential: a node holds an int and any number of child nodes."
  [:schema {:registry {::node [:map
                               [:v [:int {:min 0 :max 100}]]
                               [:kids {:optional true}
                                [:vector {:max 2} [:ref ::node]]]]}}
   ::node])

(defn- node-sum [{:keys [v kids]}]
  (reduce + v (map node-sum kids)))

(deftest a-local-registry-survives-every-lever
  (testing "the levers accept a schema whose refs only its own registry can resolve"
    (is (some? (hst/input-gen Node))          "input-gen")
    (is (some? (hst/output-oracle [:int]))    "output-oracle")
    (is (some? (hst/applier Node))            "applier")
    (is (false? (hst/arglist-schema? Node))   "arglist-schema? — a :schema is one value"))
  (testing "and seeded-cases produces values that actually conform"
    ;; seeded-cases returns {:case-n value}, so the VALUES are the cases.
    (let [cases  (vals (hst/seeded-cases Node 0 4))
          oracle (hst/output-oracle Node)]
      (is (seq cases))
      (is (every? oracle cases)
          "a generated case must satisfy the very schema it was generated from"))))

(deftest a-local-registry-survives-a-form-roundtrip
  ;; The sharper case: anything that goes through `m/form` loses the compiled
  ;; schema object and must rebuild from the form alone, local registry included.
  (let [round-tripped (m/form (m/schema Node))]
    (is (some? (hst/input-gen round-tripped)))
    (is (some? (hst/applier round-tripped)))
    (is (false? (hst/arglist-schema? round-tripped)))))

(deftest the-recursive-schema-actually-recurses
  ;; Negative control: if `::node` silently resolved to something trivial, the
  ;; tests above would pass while proving nothing. Demand a nested value.
  (let [cases (vals (hst/seeded-cases Node 7 60))]
    (is (some (fn [c] (seq (:kids c))) cases)
        "no generated case had children — the ref is not being followed")
    (is (some (fn [c] (some (comp seq :kids) (:kids c))) cases)
        "no case nested TWO deep — one level could come from a non-recursive schema")
    (is (every? (fn [c] (<= (:v c) (node-sum c))) cases)
        "the child sum is well-defined, so the nested values are real nodes")))

;; The synthesized facets, over a subject whose :in carries a local registry.
(hst/deftrifecta-from-schema node-sum-from-schema
  hive-schemas.test-test/node-sum
  {:in       Node
   :out      [:int {:min 0}]
   :rel      (fn [in out] (= out (node-sum in)))
   :mutation false
   :num-tests 50})
