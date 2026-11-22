(ns hive-schemas.var-seam-test
  "Invariant gate for the hive-schemas re-export facades (epic
   20260821211613-0c0e618f): a rebound leaf is visible through the facade,
   a captured fn object is stale (control), and no fn-valued frozen alias
   `(def name owner/sym)` may return to src/."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-schemas.plan :as plan]
            [hive-schemas.schema :as s]
            [hive-schemas.subject :as subject]
            [hive-spi.schema.registry :as reg]))

(defn- with-rebound-leaf
  "alter-var-root, not with-redefs: the rebinding must reach code that
   resolves the var LATER, which is what a hot reload does."
  [leaf-var replacement f]
  (let [original @leaf-var]
    (try
      (alter-var-root leaf-var (constantly replacement))
      (f)
      (finally (alter-var-root leaf-var (constantly original))))))

(deftest facade-sees-a-rebound-leaf-test
  (testing "hive-spi registry/validate rebound is seen through hive-schemas.schema/validate"
    (with-rebound-leaf #'reg/validate (fn [& _] ::rebound)
      (fn [] (is (= ::rebound (s/validate :int 1))))))
  (testing "subject/subject-symbol rebound is seen through plan/subject-symbol"
    (with-rebound-leaf #'subject/subject-symbol (fn [& _] ::rebound)
      (fn [] (is (= ::rebound (plan/subject-symbol 'a/b)))))))

(deftest frozen-copy-is-stale-control-test
  (let [frozen reg/validate]
    (with-rebound-leaf #'reg/validate (fn [& _] ::rebound)
      (fn []
        (is (= ::rebound (s/validate :int 1)) "the facade followed the var")
        (is (true? (frozen :int 1))
            "the captured object still runs the OLD validate: the staleness removed")))))

(deftest facade-still-behaves-test
  (is (true? (s/validate :int 1)))
  (is (false? (s/validate :int "x")))
  (is (some? (s/explain :int "x")))
  (is (identical? reg/registry s/registry) "registry is a value alias, kept as def"))

(def ^:private frozen-alias-pattern
  "Top-level `(def name owner/sym)`, optionally with a docstring."
  #"(?m)^\(def\s+([^\s\)\^]+)\s+(?:\"(?:[^\"\\]|\\.)*\"\s+)?([a-zA-Z][\w.\-]*/[^\s\)]+)\s*\)")

(defn- src-files []
  (->> (file-seq (io/file "src"))
       (filter #(re-find #"\.clj[c]?$" (.getName ^java.io.File %)))))

(defn- file-ns [f]
  (some->> (slurp f) (re-find #"\(ns\s+([^\s\)]+)") second symbol))

(deftest no-fn-valued-frozen-alias-in-src-test
  (let [hits (for [f (src-files)
                   :let [ns-sym (file-ns f)]
                   :when ns-sym
                   [_ n _] (re-seq frozen-alias-pattern (slurp f))]
               [ns-sym (symbol n)])]
    (is (seq (src-files)) "src/ was found (non-vacuity)")
    (let [offenders (for [[ns-sym n] hits
                          :let [_ (require ns-sym)
                                v (ns-resolve ns-sym n)]
                          :when (and v (fn? @v))]
                      (str ns-sym "/" n))]
      (is (= [] (vec (sort offenders)))
          "these defs capture a fn object instead of delegating through the var"))))
