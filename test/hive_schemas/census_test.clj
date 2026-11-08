(ns hive-schemas.census-test
  "The static census: its pure levers as hive-test trifectas, a fixture repo
   with a known answer, and the ratchet gate pointed at hive-schemas itself."
  (:require [clojure.edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.census :as census]
            [hive-schemas.test :as hst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; =============================================================================
;; Pure levers
;; =============================================================================

(defn percent-rel
  "Within half a tenth of the exact ratio, and 0.0 for an empty denominator."
  [[a b] out]
  (if (zero? b)
    (= 0.0 out)
    (<= (Math/abs (- out (/ (* 100.0 a) b))) 0.05)))

(hst/deftrifecta-from-schema percent-facets census/percent
  {:in        [:cat [:int {:min 0 :max 500}] [:int {:min 0 :max 500}]]
   :out       :double
   :rel       hive-schemas.census-test/percent-rel
   :contract  true
   :num-tests 80})

(def ^:private Row
  [:map
   [:repo [:enum "a" "b" "c"]]
   [:fns [:int {:min 0 :max 9}]] [:contracted [:int {:min 0 :max 9}]]
   [:ns [:int {:min 0 :max 9}]] [:hive-tested [:int {:min 0 :max 9}]]
   [:contract-pct [:double {:min 0.0 :max 100.0}]]
   [:facet-pct [:double {:min 0.0 :max 100.0}]]])

(defn floors-rel
  "Every row's percentages become its floor, so the rows meet their own floors."
  [rows out]
  (and (= (set (map :repo rows)) (set (keys out)))
       (empty? (census/ratchet-failures (vals (into {} (map (juxt :repo identity)) rows)) out))))

(hst/deftrifecta-from-schema floors-facets census/floors
  {:in        [:vector Row]
   :out       census/Floors
   :rel       hive-schemas.census-test/floors-rel
   :mutation  false
   :num-tests 60})

(defn ratchet-failures-rel
  "One message per floor key that is missing a row or exceeds the row's value."
  [[rows floors] out]
  (let [by-repo (into {} (map (juxt :repo identity)) rows)
        broken  (for [[repo floor] floors
                      k [:contract-pct :facet-pct]
                      :let [need (get floor k) row (get by-repo repo)]
                      :when (and need (or (nil? row) (< (get row k) need)))]
                  [repo k])]
    (= (count broken) (count out))))

(hst/deftrifecta-from-schema ratchet-failures-facets census/ratchet-failures
  {:in        [:cat [:vector Row]
               [:map-of [:enum "a" "b" "c" "d"]
                [:map [:contract-pct {:optional true} [:double {:min 0.0 :max 100.0}]]
                 [:facet-pct {:optional true} [:double {:min 0.0 :max 100.0}]]]]]
   :out       [:vector :string]
   :rel       hive-schemas.census-test/ratchet-failures-rel
   :num-tests 80})

(defn parse-args-rel
  "Flags are consumed with their values (a flag's value may itself look like a
   flag); every other arg is a repo, and no repo means the current directory."
  [args out]
  (let [[plain detail?] (loop [[a & more] args acc [] detail? false]
                          (cond (nil? a) [acc detail?]
                                (= "--detail" a) (recur more acc true)
                                (contains? #{"--ratchet" "--record"} a) (recur (rest more) acc detail?)
                                :else (recur more (conj acc a) detail?)))]
    (and (= (:repos out) (if (seq plain) plain ["."]))
         (= (:detail? out) detail?))))

(hst/deftrifecta-from-schema parse-args-facets census/parse-args
  {:in        [:vector [:enum "--detail" "--ratchet" "--record" "x" "y" "f.edn"]]
   :out       [:map [:repos [:vector :string]] [:detail? :boolean]]
   :rel       hive-schemas.census-test/parse-args-rel
   :num-tests 80})

;; =============================================================================
;; A fixture repo with a known answer
;; =============================================================================

(defn- write! [root rel text]
  (let [f (io/file root rel)]
    (io/make-parents f)
    (spit f text)))

(defn- fixture-repo []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "census-fixture" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (write! root "src/fx/a.cljc"
            (str "(ns fx.a (:require [malli.core :as mc] [fx.b :as b]))\n"
                 "(defn one [x] x)\n"
                 "(defn ^{:private true\n       :doc \"multi-line meta\"} hidden [x] x)\n"
                 "(defn- also-hidden [x] x)\n"
                 "#?(:clj (defn two [x] x))\n"
                 "#?@(:clj [(defn three [x] x)])\n"
                 "(defn four {:malli/schema [:=> [:cat :int] :int]} [x] x)\n"
                 "(def kw ::b/thing)\n"
                 "(mc/=> one [:=> [:cat :int] :int])\n"
                 "(mc/=> b/other [:=> [:cat :int] :int])\n"))
    (write! root "src/fx/b.clj"
            (str "(ns fx.b)\n(defn other [x] x)\n(defn bare [x] x)\n"))
    (write! root "src/fx/broken.clj" "(ns fx.broken (defn")
    (write! root "test/fx/a_test.clj"
            "(ns fx.a-test (:require [fx.a]))\n(deftrifecta-from-schema t fx.a/one {})\n")
    (write! root "test/fx/b_more_test.clj"
            "(ns fx.b-more-test)\n(deftest-golden g {})\n")
    root))

(deftest repo-census-reads-what-regexes-missed
  (let [row (census/repo-census (fixture-repo) {:name "fx"})
        per (into {} (map (juxt :ns identity)) (:per row))]
    (testing "metadata in any shape, reader conditionals and splices"
      (is (= #{"one" "two" "three" "four"} (:defns (per "fx.a")))))
    (testing "contracts: aliased m/=>, remote m/=>, :malli/schema"
      (is (= #{"one" "four"} (:covered (per "fx.a"))))
      (is (= #{"other"} (:covered (per "fx.b")))))
    (testing "facets credit by require or exact -test name, never by prefix"
      (is (= #{:trifecta} (:facets (per "fx.a"))))
      (is (= #{} (:facets (per "fx.b")))))
    (testing "totals and the unreadable file"
      (is (= {:fns 6 :contracted 3 :ns 2 :hive-tested 1 :contract-pct 50.0 :facet-pct 50.0}
             (select-keys row [:fns :contracted :ns :hive-tested :contract-pct :facet-pct])))
      (is (= 1 (count (:unreadable row)))))))

(deftest repo-census-counts-hive-test-contracts
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "census-hc" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (write! root "src/hc/a.cljc"
            (str "(ns hc.a (:require [hive-test.contract :as hc]))\n"
                 "(defn one [x] x)\n(defn two [x] x)\n(defn three [x] x)\n"
                 "(hc/=> one [:=> [:cat :int] :int])\n"
                 "(hive-test.contract/=> two [:=> [:cat :int] :int])\n"))
    (write! root "src/hc/b.clj"
            (str "(ns hc.b (:require [hive-test.contract :refer [=>]]))\n"
                 "(defn four [x] x)\n(=> four [:=> [:cat :int] :int])\n"))
    (write! root "src/hc/c.clj"
            (str "(ns hc.c (:require [other.lib :refer [=>]]))\n"
                 "(defn five [x] x)\n(=> five [:=> [:cat :int] :int])\n"))
    (let [per (into {} (map (juxt :ns identity)) (:per (census/repo-census root {:name "hc"})))]
      (testing "hc/=> through an alias and the full ns name"
        (is (= #{"one" "two"} (:covered (per "hc.a")))))
      (testing "=> referred from hive-test.contract"
        (is (= #{"four"} (:covered (per "hc.b")))))
      (testing "an unqualified => referred from any other ns is not a contract"
        (is (= #{} (:covered (per "hc.c"))))))))

(deftest census3-script-counts-hive-test-contracts
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "census3-hc" (make-array java.nio.file.attribute.FileAttribute 0)))
        repo (io/file root "hcrepo")]
    (write! repo "deps.edn" "{}")
    (write! repo "src/hc/a.clj"
            (str "(ns hc.a (:require [hive-test.contract :as hc]))\n"
                 "(defn one [x] x)\n(defn two [x] x)\n"
                 "(hc/=> one [:=> [:cat :int] :int])\n"))
    (write! repo "src/hc/b.clj"
            (str "(ns hc.b (:require [hive-test.contract :refer [=>]]))\n"
                 "(defn three [x] x)\n(=> three [:=> [:cat :int] :int])\n"))
    (let [{:keys [exit out err]} (shell/sh "bb" "script/census/census3.clj" "--edn"
                                           (.getPath root) "hcrepo")
          per (get (clojure.edn/read-string out) "hcrepo")]
      (is (zero? exit) err)
      (is (= #{"one"} (:covered (per "hc.a"))))
      (is (= #{"three"} (:covered (per "hc.b")))))))

;; =============================================================================
;; The ratchet, pointed at hive-schemas
;; =============================================================================

(census/deftest-census-ratchet hive-schemas-holds-its-floors
  {:repo        "."
   :name        "hive-schemas"
   :src-paths   ["src" "synth"]
   :floors-file "census-floors.edn"})
