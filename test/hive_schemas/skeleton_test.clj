(ns hive-schemas.skeleton-test
  "The test-skeleton generator, pointed at hive-schemas' own census namespace."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.census :as census]
            [hive-schemas.skeleton :as sk]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private census-file "src/hive_schemas/census.clj")

(deftrifecta-from-schema effectful-names hive-schemas.skeleton/effectful?
  {:in  [:cat [:enum "-main" "main" "run!" "run" "a!b" "x!" ""]]
   :out :boolean
   :rel (fn [[s] out] (= out (or (= "-main" s) (str/ends-with? s "!"))))})

(deftrifecta-from-schema arities-of-a-literal-function-schema hive-schemas.skeleton/schema-arities
  {:in  [:cat [:enum [:=> [:cat :int] :int]
               [:=> [:cat] :nil]
               [:function [:=> [:cat :any] :any] [:=> [:cat :any :map] :any]]
               :int
               nil]]
   :out [:vector sk/Arity]
   :rel (fn [[form] out]
          (= (mapv :arity out)
             (cond (not (vector? form))         []
                   (= :function (first form)) [1 2]
                   :else                      [(count (rest (second form)))])))})

(deftest the-census-skeleton
  (let [skel   (sk/skeleton-of census-file {:paths [census-file]})
        by-name (into {} (map (juxt :name identity)) (:trifectas skel))]
    (testing "one trifecta per pure contracted arity, none for -main"
      (is (= 11 (count (:trifectas skel))))
      (is (= #{"-main"} (:effectful skel)))
      (is (= '#{repo-census-arity1 repo-census-arity2}
             (into #{} (filter #(str/includes? (str %) "-arity")) (keys by-name)))))
    (testing "schema symbols are qualified for a namespace without the source's aliases"
      (is (= '[:cat [:sequential hive-schemas.census/CensusRow]] (:in (by-name 'floors))))
      (is (= 'hive-schemas.census/CensusRow (:out (by-name 'repo-census-arity1)))))
    (testing "the gate covers the file and census is fully contracted"
      (is (= {:name 'census-contracts :paths [census-file] :exempt {}} (:gate skel))))
    (testing "the rendered namespace reads back as forms"
      (let [f     (doto (java.io.File/createTempFile "skeleton" ".clj") (.deleteOnExit))
            _     (spit f (sk/render skel))
            forms (census/read-forms f)]
        (is (= '(ns hive-schemas.census-test) (take 2 (first forms))))
        (is (= 11 (count (filter #(= 'deftrifecta-from-schema (first %)) forms))))
        (is (= 'coverage/deftest-contract-coverage (first (last forms))))))))

(deftest uncontracted-fns-become-blank-exemptions
  (let [inv  (sk/inventory '[(ns a.b (:require [malli.core :as m] [x.y :as xy]))
                             (def ^:private Hidden :int)
                             (def Shown [:map [:k :int]])
                             (defn f [x] x)
                             (defn g [x] x)
                             (defn h! [x] x)
                             (defn- p [x] x)
                             (m/=> f [:=> [:cat Hidden xy/K] Shown])
                             (m/=> h! [:=> [:cat :any] :nil])])
        skel (sk/skeleton inv {:paths ["src/a/b.clj"]})]
    (is (= #{"g"} (:uncontracted inv)))
    (is (= {'a.b/g ""} (get-in skel [:gate :exempt])))
    (is (= #{"h!"} (:effectful skel)))
    (is (= [{:name 'f :subject 'a.b/f
             :in [:cat '(deref (var a.b/Hidden)) 'x.y/K]
             :out 'a.b/Shown}]
           (:trifectas skel)))
    (is (some #{['x.y]} (:requires skel)) "a namespace a schema names is required")))

(deftest parse-args-reads-files-and-paths
  (is (= {:files ["a.clj" "b.clj"] :paths ["src/a" "src/b"]}
         (sk/parse-args ["a.clj" "--paths" "src/a,src/b" "b.clj"]))))
