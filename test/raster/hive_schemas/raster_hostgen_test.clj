(ns hive-schemas.raster-hostgen-test
  "The host binding as the Nth projection of one kernel spec, not the second copy
   of a hand-written one.

   raster ships a cljs host emitter. The parts of it that are not about
   ClojureScript — the 8-aligned region layout and the resolution of a `:call`
   entry to a wasm argument — live once in `hive-schemas.raster.hostgen`, and
   the same layout drives BOTH the in-JVM call plan and the generated cljw
   source. The load-bearing test is that those two agree on real bytes."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [hive-schemas.raster :as hr]
            [hive-schemas.raster.backends :as bk]
            [hive-schemas.raster.hostgen :as hg]
            [raster.compiler.pipeline :as pl]
            [raster.dl.nn :as nn]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def cljw-binary
  (or (System/getenv "CLJW_BIN") (System/getProperty "cljw.bin") "cljw"))

(def n 4)
(def a-vals [0.5 -1.5 2.25 3.0])
(def b-vals [0.25 -0.5 1.5 4.0])
(def zeros (vec (repeat n 0.0)))
(def expected (mapv + a-vals b-vals))

(def regions
  "Three f32 scratch buffers, sized generously. Their OFFSETS are never written
   here — that is what lay-out is for."
  '[{:sym A :view :f32 :bytes 64}
    {:sym B :view :f32 :bytes 64}
    {:sym OUT :view :f32 :bytes 64}])

(def kernel-spec
  '{:export "residual_add" :fn "residual-add!" :args [a b out n]
    :call [[:in A a] [:in B b] [:inout OUT out] n]})

(def layout (hg/lay-out nil regions nil))

(defn- module-bytes ^bytes []
  (byte-array (:bytes (pl/compile-wasm #'nn/residual-add! :name "residual_add" :dtype :float))))

(deftest lay-out-assigns-eight-aligned-offsets-in-declaration-order
  (testing "offsets are derived, ascending and 8-aligned — an offset written by
           hand in one host and recomputed in another is two definitions of the
           same layout"
    (is (= 0 (hg/byte-off layout 'A)))
    (is (= 64 (hg/byte-off layout 'B)))
    (is (= 128 (hg/byte-off layout 'OUT)))
    (is (every? #(zero? (mod (hg/byte-off layout %) 8)) '[A B OUT])))
  (testing "a region sized from its data rather than :bytes lays out too"
    (let [l (hg/lay-out '[{:sym HP :view :i32 :data [1 2 3]}] '[{:sym S :view :f64 :bytes 8}] nil)]
      (is (= 12 (:bytes (l 'HP))))
      (is (= 16 (hg/byte-off l 'S)) "12 bytes of consts round up to 16"))))

(deftest resolve-call-arg-maps-a-region-to-its-offset-and-passes-symbols-through
  (is (= 0 (hg/resolve-call-arg layout '[:in A a])))
  (is (= 128 (hg/resolve-call-arg layout '[:inout OUT out])))
  (is (= 7 (hg/resolve-call-arg layout '[:lit 7])))
  (is (= 'n (hg/resolve-call-arg layout 'n))
      "a bare symbol is a pass-through the caller substitutes, not an offset"))

(deftest emit-host-refuses-a-host-nobody-implemented
  (testing "hosts are an OPEN set, so a new one is a defmethod — and an unknown
           one throws rather than emitting an empty binding"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no host emitter for"
                          (hg/emit-host :fortran {:ns 'x :layout layout :kernels []})))))

(deftest the-plan-and-the-generated-host-agree-on-real-bytes
  (testing "THE POINT OF THE SPLIT: one spec and one layout drive both the
           in-JVM call plan and the generated cljw namespace. If they ever
           disagreed about an offset, this is where it would show"
    (let [backend (get (bk/available-backends) :cljw-interp)]
      (is (some? backend)
          "needs a wasm-enabled cljw carrying the linear-memory surface; set CLJW_BIN")
      (when backend
        (let [bytes (module-bytes)
              plan  (hg/call-plan layout kernel-spec
                                  '{a [0.5 -1.5 2.25 3.0]
                                    b [0.25 -0.5 1.5 4.0]
                                    out [0.0 0.0 0.0 0.0]
                                    n 4})]
          (testing "the plan's arguments are the laid-out offsets, not literals
                   someone typed"
            (is (= [0 64 128 4] (:args plan))))
          (testing "and running it reproduces the reference"
            (let [got (first (:reads (bk/run-plan backend bytes plan)))]
              (is (hr/approx-seq= expected got 1.0e-6)))))))))

(deftest a-generated-cljw-namespace-is-required-and-called-by-the-real-binary
  (testing "the card's acceptance: a raster array kernel, a GENERATED cljw
           namespace, required and called from the cljw binary, matching the
           JVM reference"
    (let [backend (get (bk/available-backends) :cljw-interp)]
      (is (some? backend) "needs a wasm-enabled cljw; set CLJW_BIN")
      (when backend
        (let [dir  (java.io.File/createTempFile "hostgen" "")
              _    (do (.delete dir) (.mkdirs (io/file dir "src/gen")))
              wasm (io/file dir "k.wasm")]
          (try
            (with-open [o (io/output-stream wasm)] (.write o (module-bytes)))
            (spit (io/file dir "deps.edn") "{:paths [\"src\"]}\n")
            (spit (io/file dir "src/gen/kernels.clj")
                  (hg/emit-host :cljw {:ns 'gen.kernels
                                       :module-path (.getAbsolutePath wasm)
                                       :layout layout
                                       :kernels [kernel-spec]}))
            (spit (io/file dir "run.clj")
                  (str "(require '[gen.kernels :as k])\n"
                       "(k/init! :interp)\n"
                       "(println \"RESULT\" (pr-str (k/residual-add! "
                       (pr-str a-vals) " " (pr-str b-vals) " " (pr-str zeros) " " n ")))\n"))
            (let [{:keys [exit out err]} (shell/sh cljw-binary "run.clj" :dir dir)
                  line (some #(when (str/starts-with? % "RESULT ") (subs % 7))
                             (str/split-lines out))]
              (is (zero? exit) (str "cljw exited " exit ": " err))
              (is (some? line) (str "no RESULT line. out=" out " err=" err))
              (when line
                (let [{:keys [out]} (edn/read-string line)]
                  (is (hr/approx-seq= expected (:OUT out) 1.0e-6)
                      (str "generated binding disagrees with the reference: " out)))))
            (finally
              (doseq [f (reverse (file-seq dir))] (.delete f)))))))))
