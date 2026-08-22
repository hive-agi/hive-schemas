(ns hive-schemas.source-roots-test
  "The two enumerated surfaces a new sibling source root has to be added to —
   version.edn `:src-dirs` and release.yml's push paths — checked against the
   roots actually on disk.

   Runs in the DEFAULT lane and needs none of the optional layers' deps: it
   reads the repo's own configuration, not its code."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private repo-root
  (loop [d (.getCanonicalFile (io/file "."))]
    (cond (nil? d)                            nil
          (.isFile (io/file d "version.edn")) d
          :else                               (recur (.getParentFile d)))))

(defn- source-root?
  "A top-level directory is a SHIPPED source root when it holds a `hive_schemas`
   package and is not a test root."
  [^java.io.File d]
  (and (.isDirectory d)
       (not (str/starts-with? (.getName d) "test"))
       (.isDirectory (io/file d "hive_schemas"))))

(defn- roots-on-disk []
  (->> (.listFiles (io/file repo-root)) (filter source-root?) (map #(.getName ^java.io.File %)) sort vec))

(defn- declared-src-dirs []
  (set (:src-dirs (edn/read-string (slurp (io/file repo-root "version.edn"))))))

(defn- release-trigger-paths
  "The path globs release.yml's push trigger fires on."
  []
  (into #{}
        (comp (map str/trim)
              (keep #(second (re-matches #"- '(.*)'" %))))
        (str/split-lines (slurp (io/file repo-root ".github/workflows/release.yml")))))

(deftest the-repo-root-is-locatable
  (is (some? repo-root)
      "no version.edn walking up from the working directory — this suite reads the
       repo's own configuration and cannot run without it"))

(deftest every-source-root-on-disk-is-shipped
  (let [declared (declared-src-dirs)]
    (doseq [r (roots-on-disk)]
      (is (contains? declared r)
          (str "source root `" r "/` is on disk but absent from version.edn "
               ":src-dirs — the jar will not carry it, and every LOCAL check "
               "still passes because the alias reads the working tree")))))

(deftest every-source-root-on-disk-triggers-a-release
  (let [globs (release-trigger-paths)]
    (is (seq globs) "no path globs parsed out of release.yml — the check below
                     would pass vacuously if the trigger were reshaped")
    (doseq [r (roots-on-disk)]
      (is (contains? globs (str r "/**"))
          (str "source root `" r "/` is on disk but absent from release.yml push "
               "paths — a commit touching only it mints no version")))))

(deftest a-test-root-is-never-shipped
  (let [declared (declared-src-dirs)]
    (doseq [^java.io.File d (.listFiles (io/file repo-root))
            :when (and (.isDirectory d) (str/starts-with? (.getName d) "test"))]
      (is (not (contains? declared (.getName d)))
          (str "test root `" (.getName d) "/` is named in :src-dirs — tests would ship")))))

(deftest the-guard-is-not-vacuous
  (testing "an empty root set satisfies every assertion above"
    (is (<= 5 (count (roots-on-disk)))))
  (testing "and the discriminator finds an OPTIONAL layer, not only src"
    (is (contains? (set (roots-on-disk)) "raster"))))
