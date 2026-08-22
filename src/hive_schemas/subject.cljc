(ns hive-schemas.subject
  "How a schema-shaped value reaches the subject it schematizes.

     schema-arity     ?s -> :arglist | :value
     arglist-schema?  ?s -> boolean
     applier          ?s -> (fn [subject in] ...)
     subject-symbol   ns/fn | #'ns/fn | var -> the bare qualified symbol"
  (:require [hive-spi.schema.registry :as reg]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defmulti schema-arity
  "How a value generated from an `:in` schema reaches the subject:
     :arglist — the value IS an argument list; `apply` it
     :value   — the value is one argument; pass it directly

   Dispatches on the malli schema type; extend with `defmethod`."
  (fn [?schema] (m/type (reg/schema ?schema))))

(defmethod schema-arity :cat [_] :arglist)
(defmethod schema-arity :catn [_] :arglist)
(defmethod schema-arity :default [_] :value)

(defn arglist-schema?
  "True when `?schema` describes an argument LIST rather than a single value."
  [?schema]
  (= :arglist (schema-arity ?schema)))

(defn applier
  "(fn [subject in] ...) applying a generated `:in` value to `subject` according
   to `schema-arity`."
  [?schema]
  (if (arglist-schema? ?schema)
    (fn [f in] (apply f in))
    (fn [f in] (f in))))

(defn- var-symbol
  "Qualified symbol naming the var `x`, or nil when `x` is not a var."
  [x]
  #?(:clj     (when (var? x)
                (let [m (meta x)]
                  (symbol (str (ns-name (:ns m))) (str (:name m)))))
     :default (when (var? x) nil)))

(defn subject-symbol
  "The bare qualified symbol behind `subject`. Accepts `ns/fn`, `#'ns/fn`,
   `'ns/fn` and (on the JVM) a var. Throws on anything else."
  [subject]
  (or (var-symbol subject)
      (cond
        (symbol? subject)
        subject

        (and (seq? subject) (contains? #{'var 'quote} (first subject)))
        (subject-symbol (second subject))

        :else
        (throw (ex-info "Not a subject: want ns/fn, #'ns/fn or a var"
                        {:error :subject/unrecognized :subject subject})))))
