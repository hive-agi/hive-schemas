(ns hive-schemas.raster.hostgen
  "The HOST-AGNOSTIC half of a raster kernel's host binding, plus an open
   projection to one host's source.

   raster ships `raster.compiler.cljs-emit`, which is this whole job fused for a
   single host. Two of its pieces are not about ClojureScript at all — the
   8-aligned region layout and the resolution of a `:call` entry to a wasm
   argument — and they are what is reproduced here, once. Only `emit-host`
   differs per host, and hosts are an OPEN set (cljs, cljw, JVM-Chicory, cljrs),
   so it is a multimethod rather than a case.

   The region and call vocabulary is raster's, deliberately unchanged, so one
   spec drives every host:

     :consts   [{:sym HP :data <nums> :view :i32}]   baked in at init
     :scratch  [{:sym POS :view :f64 :bytes 24}]     per-call marshal buffers
     :resident [{:sym W :view :i32 :bytes N}]        uploaded once
     :call     entries — sym | [:lit v] | [:const S] | [:resident S]
                        | [:in S arg] | [:inout S arg]

   Levers:
     lay-out      consts+scratch+resident -> {sym {:off :view :bytes :data :kind}}
     call-plan    layout + kernel spec + arg values -> a backends call plan
     emit-host    host id + binding -> that host's source"
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def dtype-bytes
  "Width in bytes of each element view."
  {:i8 1 :u8 1 :i16 2 :u16 2 :i32 4 :u32 4 :f32 4 :i64 8 :f64 8})

(defn align8
  "Round `n` up to an 8-byte boundary."
  ^long [^long n]
  (* 8 (quot (+ n 7) 8)))

(defn lay-out
  "Assign 8-aligned byte offsets to const + scratch + resident regions, in that
   order. Returns `{sym {:off :view :bytes :data :kind}}`.

   No caller hand-computes an offset, which is the point: an offset written by
   hand in one host and recomputed in another is two definitions of the same
   layout."
  [consts scratch resident]
  (loop [acc {} off 0
         regs (concat (map #(assoc % :kind :const) consts)
                      (map #(assoc % :kind :scratch) scratch)
                      (map #(assoc % :kind :resident) resident))]
    (if-let [{:keys [sym view bytes data] :as r} (first regs)]
      (let [nbytes (long (or bytes (* (count data) (dtype-bytes view))))]
        (recur (assoc acc sym {:off off :view view :bytes nbytes :data data :kind (:kind r)})
               (align8 (+ off nbytes))
               (rest regs)))
      acc)))

(defn byte-off
  "Byte offset of region `sym`."
  [layout sym]
  (:off (layout sym)))

(defn region-view
  "Element view of region `sym`."
  [layout sym]
  (:view (layout sym)))

(defn marshalled
  "The `:call` entries that move an argument through a region, as
   `[[direction sym arg] ...]` with direction `:in` or `:inout`."
  [call]
  (vec (filter (fn [c] (and (vector? c) (#{:in :inout} (first c)))) call)))

(defn resolve-call-arg
  "One `:call` entry -> the value the wasm export receives. A memory region
   resolves to its BYTE OFFSET; a bare symbol is a pass-through argument and is
   returned as-is for a caller to substitute."
  [layout c]
  (cond
    (symbol? c) c
    (vector? c) (case (first c)
                  :lit         (second c)
                  :const       (byte-off layout (second c))
                  :resident    (byte-off layout (second c))
                  (:in :inout) (byte-off layout (nth c 1)))
    :else c))

(defn call-plan
  "The engine-neutral plan `hive-schemas.raster.backends/run-plan` executes, for
   ONE invocation of kernel spec `k` under `layout`.

   `values` maps a pass-through symbol or a marshalled argument symbol to its
   value. Every `:in` / `:inout` region is written; every `:inout` region is read
   back, sized from the value it was given."
  [layout {:keys [export call]} values]
  (let [moved (marshalled call)]
    {:export export
     :args   (mapv (fn [c]
                     (let [r (resolve-call-arg layout c)]
                       (if (symbol? r) (get values r) r)))
                   call)
     :writes (mapv (fn [[_ sym arg]]
                     {:at (byte-off layout sym)
                      :dtype (region-view layout sym)
                      :values (vec (get values arg))})
                   moved)
     :reads  (into []
                   (comp (filter (fn [[dir _ _]] (= :inout dir)))
                         (map (fn [[_ sym arg]]
                                {:at (byte-off layout sym)
                                 :dtype (region-view layout sym)
                                 :count (count (get values arg))})))
                   moved)}))

;; ---------------------------------------------------------------------------
;; The per-host projection
;; ---------------------------------------------------------------------------

(defmulti emit-host
  "Project a laid-out kernel binding to ONE host's source.

   `binding` is `{:ns :module-path :layout :kernels [{:export :fn :args :call}]}`.
   Hosts are an OPEN set, so a new host is a `defmethod`, never a branch here."
  (fn [host _binding] host))

(defmethod emit-host :default [host _]
  (throw (ex-info (str "no host emitter for " host)
                  {:host host :known (vec (keys (methods emit-host)))})))

(defn- cljw-wrapper
  "One kernel's cljw wrapper.

   Where the cljs host and this one genuinely differ: cljs marshals an `:inout`
   region back by mutating the caller's typed array in place. A cljw vector is
   immutable, so the wrapper RETURNS what it read instead, uniformly as
   `{:result <call return> :out {:SYM [...]}}` — the same shape whether there
   are zero, one or many outputs, because a shape that depends on the count is
   a branch every caller then has to repeat."
  [layout {:keys [export args call] fname :fn}]
  (let [moved   (marshalled call)
        writes  (str/join "\n    "
                          (for [[_ sym arg] moved]
                            (format "(wasm/mem-write! m %s %d %s)"
                                    (region-view layout sym) (byte-off layout sym) arg)))
        cargs   (str/join " " (map #(pr-str (resolve-call-arg layout %)) call))
        reads   (str/join "\n                       "
                          (for [[dir sym arg] moved :when (= :inout dir)]
                            (format ":%s (wasm/mem-read m %s %d (count %s))"
                                    sym (region-view layout sym) (byte-off layout sym) arg)))]
    (format "(defn %s [%s]\n  (let [m @M]\n    %s\n    (let [r (wasm/call m \"%s\" %s)]\n      {:result r\n       :out {%s}})))"
            fname (str/join " " args) writes export cargs reads)))

(defmethod emit-host :cljw
  [_ {:keys [ns module-path layout kernels]}]
  (str ";; GENERATED by hive-schemas.raster.hostgen — do not edit.\n"
       ";; cljw counterpart of a raster kernel module: the (ptr,len) marshaling\n"
       ";; against the offsets lay-out assigned, so no caller computes one.\n"
       "(ns " ns ")\n\n"
       "(defonce M (atom nil))\n\n"
       "(defn init!\n"
       "  ([] (init! :interp))\n"
       "  ([engine] (reset! M (wasm/load " (pr-str module-path) " {:engine engine}))))\n\n"
       "(defn ready? [] (some? @M))\n\n"
       (str/join "\n\n" (map #(cljw-wrapper layout %) kernels))
       "\n"))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> align8 [:=> [:cat :int] :int])
(m/=> lay-out [:=> [:cat [:maybe [:sequential :map]] [:maybe [:sequential :map]]
                    [:maybe [:sequential :map]]] :map])
(m/=> call-plan [:=> [:cat :map :map :map] :map])
