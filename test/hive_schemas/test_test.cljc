(ns hive-schemas.test-test
  "Proves the schema -> free-tests bridge: runtime levers (unit) + the
   synthesized property/mutation facets running as real tests."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-spi.schema.registry :as reg]
            [hive-test.trifecta :as tri]))

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