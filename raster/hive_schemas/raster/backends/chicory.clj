(ns hive-schemas.raster.backends.chicory
  "OPTIONAL: the Chicory in-JVM wasm engine as an `IWasmBackend`.

   Loads only when com.dylibso.chicory/runtime is on the classpath — the
   `:import` below fails loudly otherwise, which is the point: a backend that
   cannot load must not quietly become one fewer engine.

   Levers:
     chicory-backend  -> an IWasmBackend over Chicory"
  (:require [hive-schemas.raster.backends :as bk])
  (:import [com.dylibso.chicory.wasm Parser]
           [com.dylibso.chicory.runtime Instance]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn- write-elems!
  [mem at dtype values]
  (let [vs (vec values)]
    (dotimes [i (count vs)]
      (let [v (nth vs i)]
        (case dtype
          :f32 (.writeF32 mem (+ at (* 4 i)) (float v))
          :f64 (.writeF64 mem (+ at (* 8 i)) (double v))
          :i32 (.writeI32 mem (+ at (* 4 i)) (int v))
          :i64 (.writeLong mem (+ at (* 8 i)) (long v))
          (throw (ex-info (str "chicory backend has no writer for " dtype)
                          {:dtype dtype})))))))

(defn- read-elems
  [mem at dtype n]
  (mapv (fn [i]
          (case dtype
            :f32 (double (.readFloat mem (+ at (* 4 i))))
            :f64 (.readDouble mem (+ at (* 8 i)))
            :i32 (long (.readInt mem (+ at (* 4 i))))
            :i64 (.readLong mem (+ at (* 8 i)))
            (throw (ex-info (str "chicory backend has no reader for " dtype)
                            {:dtype dtype}))))
        (range n)))

(defn- ->raw
  "Chicory's `.apply` takes a long[], so a floating-point argument travels as
   its RAW BITS. cljw marshals scalars natively instead, which is the one place
   the two backends' call ABIs differ — the plan stays engine-neutral and each
   backend encodes."
  [x]
  (if (or (instance? Double x) (instance? Float x))
    (Double/doubleToRawLongBits (double x))
    (long x)))

(defrecord ChicoryBackend []
  bk/IWasmBackend
  (backend-id [_] :chicory)
  (available? [_]
    (try (Class/forName "com.dylibso.chicory.runtime.Instance") true
         (catch Throwable _ false)))
  (run-plan [_ wasm-bytes {:keys [export args writes reads]}]
    (let [inst (-> (Instance/builder (Parser/parse ^bytes wasm-bytes)) (.build))
          mem  (.memory inst)]
      (doseq [{:keys [at dtype values]} writes]
        (write-elems! mem at dtype values))
      (let [call (try
                   (let [r (.apply (.export inst export) (long-array (map ->raw args)))]
                     (when (and r (pos? (alength ^longs r))) (vec r)))
                   (catch Throwable t {:error (or (.getMessage t) (str (class t)))}))]
        {:call call
         :reads (mapv (fn [{:keys [at dtype count]}] (read-elems mem at dtype count)) reads)}))))

(defn chicory-backend
  "Chicory as an `IWasmBackend`."
  []
  (->ChicoryBackend))
