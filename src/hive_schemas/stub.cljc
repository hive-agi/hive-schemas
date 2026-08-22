(ns hive-schemas.stub
  "Schema-driven test doubles: conformant values, a recording decorator that
   validates both ends, and traces that become golden cases.

   Levers:
     stub              ?out -> one conformant value (deterministic)
     stub-seq          ?out n -> n conformant values
     stub-fn           ?in ?out -> a fn validating input, returning conformant
                       output; deterministic per call index
     default-provider  {method ?schema} -> (fn [method] conformant-value)
     spy               f {:in :out} -> a recording, validating decorator
     calls             spy -> the recorded calls
     violations        spy -> the calls whose input or output did not conform
     trace->cases      calls -> {label {:in .. :out ..}}"
  (:require [hive-schemas.subject :as subject]
            [hive-spi.schema.gen :as sgen]
            [hive-spi.schema.registry :as reg]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private default-seed 0)
(def ^:private default-size 10)

;; =============================================================================
;; Conformant values
;; =============================================================================

(defn stub
  "One value conforming to `?schema`. Deterministic in `:seed`."
  ([?schema] (stub ?schema {}))
  ([?schema {:keys [seed size] :or {seed default-seed size default-size}}]
   (sgen/generate ?schema {:seed seed :size size})))

(defn stub-seq
  "`n` values conforming to `?schema`, reproducible under `:seed`."
  ([?schema n] (stub-seq ?schema n {}))
  ([?schema n {:keys [seed] :or {seed default-seed}}]
   (vec (vals (sgen/seeded-cases ?schema seed n)))))

;; =============================================================================
;; Stub functions
;; =============================================================================

(defn- input-violation
  "nil when `args` satisfy `?in`, else an explanation. An arglist `?in` is
   checked against the whole argument vector; any other schema against the sole
   argument."
  [?in args]
  (let [value (if (subject/arglist-schema? ?in) (vec args) (first args))]
    (when-not (reg/validate ?in value)
      {:value value :explanation (reg/explain ?in value)})))

(defn stub-fn
  "A fake implementation of a schematized function.

   Validates its arguments against `?in` and throws `:stub/input-violation` on a
   violation. Returns a value conforming to `?out`, varying deterministically
   with the call index."
  ([?in ?out] (stub-fn ?in ?out {}))
  ([?in ?out {:keys [seed size] :or {seed default-seed size default-size}}]
   (let [calls (atom 0)]
     (fn [& args]
       (when-let [bad (input-violation ?in args)]
         (throw (ex-info "Stub called with an input its :in schema rejects"
                         (assoc bad :error :stub/input-violation :in ?in))))
       (let [i (dec (swap! calls inc))]
         (stub ?out {:seed (+ seed i) :size size}))))))

(defn default-provider
  "(fn [method] conformant-value) over `method->schema` — the seam a structural
   stub calls to fill a method it has no override for.

   An unknown method throws `:stub/unknown-method`."
  ([method->schema] (default-provider method->schema {}))
  ([method->schema opts]
   (fn [method]
     (if-let [entry (find method->schema method)]
       (stub (val entry) opts)
       (throw (ex-info "No schema declared for this stub method"
                       {:error :stub/unknown-method
                        :method method
                        :known (vec (sort-by str (keys method->schema)))}))))))

;; =============================================================================
;; The recording, validating decorator
;; =============================================================================

(def ^:private calls-key ::calls)

(defn spy
  "Decorate `f` so every call is recorded with whether its input and output
   conformed.

   `:in` / `:out` are optional; an absent one records `:in-ok?` / `:out-ok?` as
   nil, meaning NOT OBSERVED. Never alters what `f` returns and never throws on a
   violation."
  [f {:keys [in out]}]
  (let [recorded (atom [])]
    (with-meta
      (fn [& args]
        (let [value (if (and in (subject/arglist-schema? in)) (vec args) (first args))
              ret   (apply f args)]
          (swap! recorded conj
                 {:args    (vec args)
                  :in      value
                  :out     ret
                  :in-ok?  (when in (reg/validate in value))
                  :out-ok? (when out (reg/validate out ret))})
          ret))
      {calls-key recorded})))

(defn calls
  "The calls `spied` has recorded, oldest first."
  [spied]
  (some-> (get (meta spied) calls-key) deref))

(defn violations
  "The recorded calls whose input or output was OBSERVED and did not conform."
  [spied]
  (filterv (fn [{:keys [in-ok? out-ok?]}]
             (or (false? in-ok?) (false? out-ok?)))
           (calls spied)))

(defn trace->cases
  "`{label {:in .. :out ..}}` from recorded `calls-seq`, deduplicated by input and
   ordered by first observation."
  [calls-seq]
  (into (sorted-map)
        (map-indexed (fn [i {:keys [in out]}]
                       [(keyword (str "case-" i)) {:in in :out out}]))
        (->> calls-seq
             (reduce (fn [{:keys [seen acc]} c]
                       (if (contains? seen (:in c))
                         {:seen seen :acc acc}
                         {:seen (conj seen (:in c)) :acc (conj acc c)}))
                     {:seen #{} :acc []})
             :acc)))
