(ns hive-schemas.raster.backends
  "OPTIONAL: run a raster kernel's WASM module on an out-of-JVM engine, so the
   JVM `deftm` is not the only thing that ever executed it.

   Engines are an OPEN set — Chicory in-JVM, zwasm via the cljw binary, a
   browser, whatever comes next — so they are a registry of records behind a
   protocol, never a `case`. Availability is MEASURED by running the thing, not
   inferred from a classpath entry or an env var.

   A call PLAN is data:

     {:export \"residual_add\"
      :args   [0 32 64 8]
      :writes [{:at 0 :dtype :f32 :values [...]}]
      :reads  [{:at 64 :dtype :f32 :count 8}]}

   Levers:
     register-backend! / backends / available-backends
     run-plan     backend + module bytes + plan -> {:call ... :reads [[...] ...]}
     compile-plan kernel var + opts -> the compiled module"
  (:require [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [malli.core :as m]
            [raster.compiler.pipeline :as pl]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defprotocol IWasmBackend
  (backend-id [this]
    "A keyword naming this backend, unique in the registry.")
  (available? [this]
    "MEASURED: true when this backend can actually execute here, established by
     running it. Never a classpath guess.")
  (run-plan [this wasm-bytes plan]
    "Execute `plan` against `wasm-bytes`. Returns
     `{:call <return value> :reads [[...] ...]}` in `:reads` order, or throws."))

(defonce ^:private registry (atom {}))

(defn register-backend!
  "Add `backend` to the registry under its own id. Returns the id."
  [backend]
  (let [id (backend-id backend)]
    (swap! registry assoc id backend)
    id))

(defn backends
  "Every registered backend, by id."
  []
  @registry)

(defn available-backends
  "The registered backends that `available?` — i.e. that ran when asked. An
   empty map is a real answer and callers must treat it as one: a differential
   test with no second engine has not been run, it has been skipped."
  []
  (into {} (filter (fn [[_ b]] (available? b))) @registry))

;; ---------------------------------------------------------------------------
;; cljw / zwasm
;; ---------------------------------------------------------------------------

(def ^:private result-marker "__RASTER_BACKEND_RESULT__")

(defn- driver-source
  "cljw source that loads `path`, applies `plan`, and prints one marked EDN line.

   The call is wrapped: an engine REFUSING (a wasm trap) is reported as
   `{:error msg}` in the payload, not as a dead subprocess, so a caller can tell
   `the engine refused` from `the harness broke`."
  [path engine {:keys [export args writes reads]}]
  (str "(def m (wasm/load " (pr-str path) " {:engine " engine "}))\n"
       (str/join "\n"
                 (for [{:keys [at dtype values]} writes]
                   (str "(wasm/mem-write! m " dtype " " at " " (pr-str (vec values)) ")")))
       "\n(def __call (try (apply wasm/call m " (pr-str export) " " (pr-str (vec args)) ")\n"
       "                 (catch Exception e {:error (.getMessage e)})))\n"
       "(println " (pr-str result-marker) " (pr-str {:call __call :reads ["
       (str/join " " (for [{:keys [at dtype count]} reads]
                       (str "(wasm/mem-read m " dtype " " at " " count ")")))
       "]}))\n"))

(defn- marked-line
  "The EDN payload on the marked line of `out`, or nil."
  [out]
  (some (fn [line]
          (when (str/starts-with? line result-marker)
            (edn/read-string (subs line (clojure.core/count result-marker)))))
        (str/split-lines out)))

(def ^:private capability-probe
  "cljw source resolving every wasm var `run-plan` uses.

   `--version` reporting `wasm` is NOT enough: a binary predating the
   linear-memory surface (ADR-0192) answers `wasm`, resolves `wasm/load`, and
   then dies on `wasm/mem-write!` in the middle of a run. Measured — an older
   cljw on PATH did exactly that. An unresolvable var is a compile error here,
   so the probe's exit status is the answer."
  (str "(println " (pr-str result-marker)
       " (boolean (and wasm/load wasm/call wasm/mem-size wasm/mem-read wasm/mem-write!)))"))

(defrecord CljwBackend [binary engine]
  IWasmBackend
  (backend-id [_] (keyword (str "cljw-" (name engine))))
  (available? [_]
    (let [probe (java.io.File/createTempFile "cljw-probe" ".clj")]
      (try
        (spit probe capability-probe)
        (let [{:keys [exit out]} (shell/sh binary (.getAbsolutePath probe)
                                           :dir (.getParentFile probe))]
          (boolean (and (zero? exit) (str/includes? out result-marker))))
        (catch Exception _ false)
        (finally (.delete probe)))))
  (run-plan [this wasm-bytes plan]
    (let [wasm (java.io.File/createTempFile "raster-kernel" ".wasm")
          drv  (java.io.File/createTempFile "raster-driver" ".clj")]
      (try
        (with-open [o (io/output-stream wasm)] (.write o ^bytes wasm-bytes))
        (spit drv (driver-source (.getAbsolutePath wasm) engine plan))
        (let [{:keys [exit out err]} (shell/sh binary (.getAbsolutePath drv)
                                               :dir (.getParentFile drv))]
          (or (marked-line out)
              (throw (ex-info (str (backend-id this) " produced no result line")
                              {:exit exit :out out :err err}))))
        (finally (.delete wasm) (.delete drv))))))

(defn cljw-backend
  "A zwasm backend driven through the `cljw` binary. `engine` is `:interp` or
   `:jit`.

   Prefer `:interp` for a correctness oracle: zwasm's JIT traps at invoke on a
   ZERO-RESULT export outside a narrow signature window (ClojureWasm D-585),
   which is exactly the shape an in-place numeric kernel has. `:jit` is
   registered too so a suite can pin that region rather than merely route
   around it."
  ([engine] (cljw-backend "cljw" engine))
  ([binary engine] (->CljwBackend binary engine)))

(defn call-error
  "The engine's refusal message from a `run-plan` result, or nil when the call
   completed. A trap is DATA here rather than an exception, so `the engine
   refused` and `the harness broke` stay distinguishable — the second still
   throws."
  [result]
  (:error (:call result)))

;; ---------------------------------------------------------------------------
;; Compiling the kernel
;; ---------------------------------------------------------------------------

(defn compile-plan
  "Compile deftm var `v` to a wasm module named `export` at `:dtype`.
   Returns raster's module map; `:bytes` is the module."
  [v export {:keys [dtype] :or {dtype :float}}]
  (pl/compile-wasm v :name export :dtype dtype))

(defn module-bytes
  "The module's bytes as a Java byte array."
  [module]
  (byte-array (:bytes module)))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> register-backend! [:=> [:cat :any] :keyword])
(m/=> backends [:=> [:cat] :map])
(m/=> available-backends [:=> [:cat] :map])
