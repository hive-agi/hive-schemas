(ns hive-schemas.test-test
  "Proves the schema -> free-tests bridge: runtime levers (unit) + the
   synthesized property/mutation facets running as real tests."
  (:require [clojure.test :refer [deftest is]]
            [hive-schemas.test :as hst]
            [hive-spi.schema.registry :as reg]))

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

;; --- the contract lever: mg/check catches a wrong :sum, passes a correct calc ---
(deftest contract-violation-test
  (is (nil?  (hst/contract-violation ::in ::out calc-rel calc)))
  (is (some? (hst/contract-violation ::in ::out calc-rel calc-bug)))
  ;; with no rel it degrades to output-conformance: a missing key is caught
  (is (some? (hst/contract-violation ::in ::out nil (fn [_] {:sum 1})))))

;; --- the headline: property + mutation + CONTRACT tests synthesized from schemas ---
(hst/deftrifecta-from-schema calc-tests hive-schemas.test-test/calc
  {:in ::in :out ::out :rel calc-rel :contract true :num-tests 50 :n-cases 5})
