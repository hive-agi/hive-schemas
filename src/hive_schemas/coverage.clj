(ns hive-schemas.coverage
  "Which functions in a source tree carry a malli function contract.

   The universe of functions is read from the FILES. Namespaces are loaded before
   the registry is read, and one that fails to load is reported rather than
   dropped.

   Levers:
     source-files       paths -> the .clj/.cljc files under them
     file-defns         file  -> {:ns sym :defns #{qualified-sym}} | {:error ..}
     contracted         -> #{qualified-sym} carrying an m/=> contract
     coverage           opts -> the full report
     coverage-failures  report -> the gate messages it fails
     deftest-contract-coverage  a four-gate clojure.test facet"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private default-extensions
  "Source extensions scanned for the universe."
  #{"clj" "cljc"})

(def ^:private defining-forms
  "Top-level forms that introduce a function a contract could cover."
  {'defn :public 'defn- :private})

;; =============================================================================
;; The universe — read from disk
;; =============================================================================

(defn- extension
  [^java.io.File f]
  (let [n (.getName f)
        i (str/last-index-of n ".")]
    (when i (subs n (inc i)))))

(defn source-files
  "Source files under `paths` whose extension is in `extensions`
   (default .clj/.cljc), sorted by path."
  ([paths] (source-files paths default-extensions))
  ([paths extensions]
   (->> paths
        (mapcat (fn [p] (file-seq (io/file p))))
        (filter #(.isFile ^java.io.File %))
        (filter #(contains? extensions (extension %)))
        (sort-by #(.getPath ^java.io.File %))
        vec)))

(defn file-defns
  "`{:file path :ns sym :defns #{qualified-sym}}` for `file`, or
   `{:file path :error message}` when it cannot be read.

   Reads with reader conditionals allowed under the :clj feature and
   `*read-eval*` off."
  ([file] (file-defns file {:include-private? false}))
  ([file {:keys [include-private?]}]
   (let [path (.getPath ^java.io.File file)
         want (if include-private? #{:public :private} #{:public})]
     (try
       (with-open [r (java.io.PushbackReader. (io/reader file))]
         (binding [*read-eval* false]
           (loop [ns-sym nil defns #{}]
             (let [form (read {:read-cond :allow :features #{:clj} :eof ::eof} r)]
               (cond
                 (= ::eof form)
                 {:file path :ns ns-sym :defns defns}

                 (and (seq? form) (= 'ns (first form)) (symbol? (second form)))
                 (recur (second form) defns)

                 (and (seq? form) ns-sym
                      (contains? defining-forms (first form))
                      (symbol? (second form))
                      (contains? want (get defining-forms (first form))))
                 (recur ns-sym (conj defns (symbol (str ns-sym) (str (second form)))))

                 :else
                 (recur ns-sym defns))))))
       (catch Exception e
         {:file path :error (str (.getSimpleName (class e)) ": " (.getMessage e))})))))

;; =============================================================================
;; The property — read from the live registry
;; =============================================================================

(defn contracted
  "Qualified symbols currently carrying a malli function schema."
  []
  (into #{}
        (mapcat (fn [[ns-sym fns]]
                  (map (fn [fn-sym] (symbol (str ns-sym) (str fn-sym))) (keys fns))))
        (m/function-schemas)))

(defn- load-namespaces
  "Require each namespace in `ns-syms`. Returns the failures as
   `[{:ns sym :error message} ...]`."
  [ns-syms]
  (into []
        (keep (fn [ns-sym]
                (try (require ns-sym) nil
                     (catch Throwable t
                       {:ns ns-sym
                        :error (str (.getSimpleName (class t)) ": " (.getMessage t))}))))
        ns-syms))

;; =============================================================================
;; Report
;; =============================================================================

(defn coverage
  "Contract coverage over `:paths`.

   opts:
     :paths             source roots to scan                       [required]
     :exempt            {qualified-sym reason-string}              (default {})
     :include-private?  count defn- too                            (default false)
     :extensions        source extensions to scan       (default #{clj cljc})
     :require-namespaces?  load each namespace first    (default true)

   `:unreadable` and `:unloadable` record what could not be observed."
  [{:keys [paths exempt include-private? extensions require-namespaces?]
    :or   {exempt {} include-private? false
           extensions default-extensions require-namespaces? true}}]
  (let [scanned    (mapv #(file-defns % {:include-private? include-private?})
                         (source-files paths extensions))
        unreadable (filterv :error scanned)
        readable   (remove :error scanned)
        ns-syms    (into (sorted-set) (keep :ns) readable)
        unloadable (if require-namespaces? (load-namespaces ns-syms) [])
        universe   (into (sorted-set) (mapcat :defns) readable)
        contracts  (contracted)
        covered    (set/intersection universe contracts)
        missing    (into (sorted-set) (set/difference universe contracts (set (keys exempt))))
        stale      (into (sorted-set)
                         (filter (fn [k] (or (not (contains? universe k))
                                             (contains? contracts k)))
                                 (keys exempt)))]
    {:paths        (vec paths)
     :namespaces   (vec ns-syms)
     :universe     universe
     :contracted   contracts
     :covered      covered
     :missing      missing
     :exempt       exempt
     :stale-exemptions stale
     :unreadable   unreadable
     :unloadable   unloadable
     :ratio        (if (seq universe)
                     (/ (double (count covered)) (count universe))
                     0.0)}))

(defn- blank-reasons
  "Exempted symbols whose reason is missing or blank."
  [exempt]
  (into (sorted-set)
        (keep (fn [[k v]] (when (or (not (string? v)) (str/blank? v)) k)))
        exempt))

(defn coverage-failures
  "The gate messages `report` fails, in order; empty when it passes.

   Gates: the universe is non-empty; nothing is unreadable or unloadable; every
   uncontracted function is exempted; every exemption carries a prose reason and
   still applies."
  [report]
  (cond-> []
    (empty? (:universe report))
    (conj (str "vacuous coverage scan — no functions found under "
               (pr-str (:paths report))))

    (seq (:unreadable report))
    (conj (str "unreadable sources: " (pr-str (mapv :file (:unreadable report)))))

    (seq (:unloadable report))
    (conj (str "namespaces that failed to load (their contracts cannot be seen): "
               (pr-str (mapv :ns (:unloadable report)))))

    (seq (:missing report))
    (conj (str "uncontracted and unexempted: " (pr-str (vec (:missing report)))))

    (seq (blank-reasons (:exempt report)))
    (conj (str "exemptions without a reason: "
               (pr-str (vec (blank-reasons (:exempt report))))))

    (seq (:stale-exemptions report))
    (conj (str "stale exemptions (absent from the universe, or now contracted): "
               (pr-str (vec (:stale-exemptions report)))))))

(defmacro deftest-contract-coverage
  "Emit `name` — a test asserting every function under `:paths` carries an
   `m/=>` contract or a REASONED exemption.

   opts are `coverage`'s, with `:exempt` a `{fully.qualified/sym \"why\"}` map.
   Fails with one message per gate that did not hold."
  [name opts]
  `(clojure.test/deftest ~name
     (let [report#   (coverage ~opts)
           failures# (coverage-failures report#)]
       (doseq [f# failures#]
         (clojure.test/is false f#))
       (clojure.test/is (empty? failures#)
                        (str "contract coverage " (:ratio report#)
                             " over " (count (:universe report#)) " functions")))))
