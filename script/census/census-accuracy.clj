;; Per-blind-spot error bars of census2 (regex) against census3 (reader).
;; bb census-accuracy.clj <root> repo...   -> one EDN map per repo.
;; Card 20261014203048-078fa409. census3 --edn supplies the reader view; the
;; census2 regexes are reproduced verbatim here so each miss can be attributed
;; to one blind spot:
;;   :a-defn-meta     public defns census2 misses because of metadata shape
;;   :a-private-leak  defns census2 counts that are private (^{:private true} / ^:private on a later line)
;;   :b-reader-cond   public defns that exist only inside #?(:clj ...)
;;   :c-contracts     contracts census3 sees that census2 does not (alias, mx/defn, attr-map)
;;   :d-facet-fp      ns census2 credits with a facet that census3 does not (prefix-only match)
;;   :d-facet-fn      ns census3 credits that census2 does not
(require '[clojure.java.io :as io] '[clojure.string :as str] '[edamame.core :as e]
         '[babashka.process :as p])

(def args *command-line-args*)
(def root (first args))
;; census3.clj lives beside this script (script/census/ in hive-schemas).
(def briefs (or (some-> *file* io/file .getAbsoluteFile .getParent)
                (str (System/getProperty "user.home") "/.local/share/hive/briefs")))

(defn files [dir]
  (when (.isDirectory (io/file dir))
    (->> (file-seq (io/file dir)) (filter #(.isFile %))
         (filter #(re-find #"\.cljc?$" (.getName %)))
         (remove #(re-find #"/(\.|target|node_modules|resources/)" (.getPath %))))))

;; --- census2 regexes, verbatim
(defn ns-of [s] (second (re-find #"\(ns\s+(?:\^\S+\s+)?([\w.\-]+)" s)))
(defn v2-defns [s] (->> (re-seq #"(?m)^\(defn\s+(?:\^[^\s]+\s+)*([^\s\[\(\)]+)" s) (map second) set))
(defn v2-contracted [s]
  (into (set (map second (re-seq #"\((?:m|malli|mc|malli\.core)/=>\s+([^\s\)]+)" s)))
        (map second (re-seq #"(?m)^\(defn\s+\^\{[^}]*:malli/schema[^}]*\}\s+([^\s\[]+)" s))))

;; --- reader view of one file under :clj, with and without reader conditionals
(def base {:all true :auto-resolve (fn [_] 'x) :readers (fn [_] identity) :regex true})
(defn forms [s rc] (try (e/parse-string-all s (merge base (if rc {:read-cond :allow :features #{:clj}} {:read-cond :preserve}))) (catch Exception _ nil)))
(defn public-names [fs]
  (set (for [f fs :when (and (seq? f) (= 'defn (first f)) (symbol? (second f)))
             :let [n (second f) a (nth f 2 nil)]
             :when (not (or (:private (meta n)) (and (map? a) (:private a))))]
         (str n))))
(defn all-defn-names [fs] (set (for [f fs :when (and (seq? f) ('#{defn} (first f)) (symbol? (second f)))] (str (second f)))))

(defn census3-edn [repo]
  (-> (p/shell {:out :string} "bb" (str briefs "/census3.clj") "--edn" root repo) :out read-string (get repo)))

(defn v2-facets [repo]
  (let [facet-re {:trifecta #"\((?:[\w.\-]+/)?deftrifecta" :golden #"\((?:[\w.\-]+/)?deftest-golden"
                  :prop #"\((?:[\w.\-]+/)?(?:defprop|defspec|defcheck)" :mutation #"\((?:[\w.\-]+/)?(?:deftest-mutation|with-mutation)"}
        tests (for [f (files (str root "/" repo "/test")) :let [s (slurp f) n (ns-of s)] :when n]
                {:ns n :s s :facets (set (for [[k re] facet-re :when (re-find re s)] k))})]
    (fn [ns]
      (let [ts (filter #(and (seq (:facets %))
                             (or (re-matches (re-pattern (str (java.util.regex.Pattern/quote ns) "(-test|\\..*-test|-.*-test)?")) (:ns %))
                                 (re-find (re-pattern (str "[\\[\\s(]" (java.util.regex.Pattern/quote ns) "[\\s\\]):]")) (:s %))))
                       tests)]
        {:any (boolean (seq ts))
         :by-prefix-only (vec (for [t ts :when (and (not (re-find (re-pattern (str "[\\[\\s(]" (java.util.regex.Pattern/quote ns) "[\\s\\]):]")) (:s t)))
                                                    (not= (:ns t) (str ns "-test")))]
                                (:ns t)))}))))

(defn measure [repo]
  (let [v3 (census3-edn repo)
        v2f (v2-facets repo)
        per (for [f (files (str root "/" repo "/src")) :let [s (slurp f) n (ns-of s)] :when n
                  :let [no-rc (forms s false) rc (forms s true)]]
              (let [v2d (v2-defns s) pub (public-names rc) pub-norc (public-names no-rc)
                    v3c (get-in v3 [n :covered] #{})
                    v2c (set (filter (v2-contracted s) v2d))]
                {:ns n
                 :a-defn-meta (vec (remove v2d pub-norc))
                 :a-private-leak (vec (remove pub (filter (all-defn-names rc) v2d)))
                 :b-reader-cond (vec (remove pub-norc pub))
                 :c-contracts (vec (remove v2c v3c))
                 :v2-fns (count v2d) :v3-fns (count (get-in v3 [n :defns] #{}))
                 :v2-cov (count v2c) :v3-cov (count v3c)
                 :v2-facet (v2f n)
                 :v3-facet (boolean (some #(get-in v3 [n %]) [:trifecta :golden :prop :mutation]))}))
        sum (fn [k] (reduce + (map k per)))
        hits (fn [k] (into {} (for [r per :when (seq (k r))] [(:ns r) (k r)])))]
    {:repo repo
     :fns {:v2 (sum :v2-fns) :v3 (sum :v3-fns)}
     :contracted {:v2 (sum :v2-cov) :v3 (sum :v3-cov)}
     :a-defn-meta (hits :a-defn-meta) :a-private-leak (hits :a-private-leak)
     :b-reader-cond (hits :b-reader-cond) :c-contracts (hits :c-contracts)
     :d-facet-fp (vec (for [r per :when (and (get-in r [:v2-facet :any]) (not (:v3-facet r)) (pos? (:v3-fns r)))]
                        [(:ns r) (get-in r [:v2-facet :by-prefix-only])]))
     :d-facet-fn (vec (for [r per :when (and (:v3-facet r) (not (get-in r [:v2-facet :any])))] (:ns r)))}))

(doseq [r (rest args)] (prn (measure r)))
