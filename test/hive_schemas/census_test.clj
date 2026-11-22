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
  "Within half a tenth of the exact ratio, and 0.0 for an empty denominator.
   The bound carries a float epsilon: a half-tie such as 7/80 = 8.75 rounds to
   8.8, and |8.8 - 8.75| is 0.0500000000000007 in doubles."
  [[a b] out]
  (if (zero? b)
    (= 0.0 out)
    (<= (Math/abs (- out (/ (* 100.0 a) b))) (+ 0.05 1e-9))))

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
;; Require-spec expansion: the dispatch table against the nested-cond original
;; =============================================================================

(defn- expand-spec-oracle
  "The pre-table expand-spec, verbatim (cyclomatic complexity 10). The table
   version must agree with it on every spec."
  [prefix spec]
  (cond
    (symbol? spec) [[(str prefix spec) nil nil]]
    (and (sequential? spec) (symbol? (first spec)))
    (let [n (str prefix (first spec)) opts (rest spec)]
      (if (and (seq opts) (or (sequential? (first opts)) (symbol? (first opts))))
        (mapcat #(expand-spec-oracle (str n ".") %) opts)
        (let [kv (partition 2 1 opts)]
          [[n
            (some (fn [[k v]] (when (contains? #{:as :as-alias} k) (str v))) kv)
            (some (fn [[k v]] (when (and (= :refer k) (sequential? v)) (mapv str v))) kv)]])))
    :else []))

(def ^:private spec-corpus
  "Require specs of every shape the census meets, plus junk it must ignore."
  '[a.b [a.b :as ab] [a [b :refer [x]]] [a.b :as ab :refer [x y]] [a.b :as-alias q]
    [a [b :as bb] c] [a b [c d [e :as ee]]] [a.b :refer :all] [a.b :refer [x] :as z]
    [] [a.b] [:a] :foo "s" nil 1 (a.b :as ab) (a (b :as bb)) [a.b :reload] [a.b :as]
    [[a]] {:a 1}])

(defn expand-spec-rel
  "The table expansion equals the original nested-cond expansion."
  [[prefix spec] out]
  (= (vec (expand-spec-oracle prefix spec)) (vec out)))

(def ^:private expand-spec #'hive-schemas.census/expand-spec)

(defn expand-spec-subject
  "The private census stage behind a public test seam, so the synthesis macro
   can name it."
  [prefix spec]
  (expand-spec prefix spec))

(hst/deftrifecta-from-schema expand-spec-facets hive-schemas.census-test/expand-spec-subject
  {:in        [:cat [:enum "" "p." "x.y."]
               [:any {:gen/elements spec-corpus}]]
   :out       [:sequential [:tuple :string [:maybe :string] [:maybe [:vector :string]]]]
   :rel       hive-schemas.census-test/expand-spec-rel
   :mutation  false
   :num-tests 120})

(deftest expand-spec-happy
  (testing "a bare lib, an aliased libspec, and a prefix list with refers"
    (is (= [["a.b" nil nil]] (expand-spec "" 'a.b)))
    (is (= [["a.b" "ab" ["x" "y"]]] (expand-spec "" '[a.b :as ab :refer [x y]])))
    (is (= [["a.b" "bb" nil] ["a.c" nil nil] ["a.d.e" "ee" nil]]
           (expand-spec "" '[a [b :as bb] c [d [e :as ee]]]))))
  (testing "agrees with the original on the whole corpus"
    (doseq [s spec-corpus]
      (is (= (vec (expand-spec-oracle "p." s)) (vec (expand-spec "p." s))) (pr-str s)))))

(deftest expand-spec-edge
  (testing "an :as-alias counts as the alias; :refer :all yields no refers"
    (is (= [["a.b" "q" nil]] (expand-spec "" '[a.b :as-alias q])))
    (is (= [["a.b" nil nil]] (expand-spec "" '[a.b :refer :all]))))
  (testing "a libspec with no options, a dangling :as, and a list instead of a vector"
    (is (= [["a.b" nil nil]] (expand-spec "" '[a.b])))
    (is (= [["a.b" nil nil]] (expand-spec "" '[a.b :as])))
    (is (= [["a.b" "ab" nil]] (expand-spec "" '(a.b :as ab)))))
  (testing "the prefix is prepended to every expanded name"
    (is (= [["p.a.b" nil nil]] (expand-spec "p." 'a.b)))))

(deftest expand-spec-error
  (testing "specs that name no lib expand to nothing rather than throwing"
    (doseq [junk [nil 1 "s" :foo {:a 1} [] [:a] '[[a]]]]
      (is (= [] (vec (expand-spec "" junk))) (pr-str junk)))))

(deftest spec-kind-stages
  (let [kind #'hive-schemas.census/spec-kind]
    (testing "happy: each shape lands in its stage"
      (is (= :lib (kind 'a.b)))
      (is (= :libspec (kind '[a.b :as ab])))
      (is (= :prefix-list (kind '[a [b :as bb]]))))
    (testing "edge: a lone lib in a vector is a libspec with no options"
      (is (= :libspec (kind '[a.b]))))
    (testing "error: junk is :none"
      (is (= :none (kind nil)))
      (is (= :none (kind [:a])))
      (is (= :none (kind "s"))))))

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

(deftest a-repo-under-a-dot-directory-is-still-counted
  (testing "only segments BELOW the repo root disqualify a file; a checkout
            living under ~/.claude or ~/.cache must not census as empty"
    (let [root (.toFile (java.nio.file.Files/createTempDirectory
                         "census-dot" (make-array java.nio.file.attribute.FileAttribute 0)))
          repo (io/file root ".hidden-parent" "repo")]
      (write! repo "src/hd/a.clj"
              (str "(ns hd.a (:require [malli.core :as m]))\n"
                   "(defn one [x] x)\n(m/=> one [:=> [:cat :int] :int])\n"))
      (write! repo "src/.skip/hd/b.clj" "(ns hd.b)\n(defn two [x] x)\n")
      (let [row (census/repo-census repo {:name "hd"})]
        (is (= 1 (:fns row)) "the dot-parent above the root does not hide hd.a")
        (is (= 1 (:contracted row)))
        (is (= ["hd.a"] (mapv :ns (:per row)))
            "a dot segment below the root still skips its files")))))

;; =============================================================================
;; The ratchet, pointed at hive-schemas
;; =============================================================================

(census/deftest-census-ratchet hive-schemas-holds-its-floors
  {:repo        "."
   :name        "hive-schemas"
   :src-paths   ["src" "synth"]
   :floors-file "census-floors.edn"})
