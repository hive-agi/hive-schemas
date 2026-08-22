(ns hive-schemas.vocab
  "The schema vocabulary hive-schemas' own `m/=>` contracts are written in.

   Registered schemas:
     :hive.schemas/schema-ref  a malli schema: a form, a registry key, or compiled
     :hive.schemas/violation   nil, or a message naming the value that broke it
     :hive.schemas/violations  violation messages, in report order
     :hive.schemas/opts        an options map
     :hive.schemas/subject-ref ns/fn | #'ns/fn | a var
     :hive.schemas/path        a filesystem path"
  (:require [hive-spi.schema.registry :as reg]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def SchemaRef
  "A malli schema as an argument: a schema form, a key registered in the hive
   registry, or an already-compiled schema."
  :any)

(def Violation
  "nil when the property holds; otherwise a message naming the value that shows
   it does not."
  [:maybe :string])

(def Violations
  "Violation messages, in the order the lever reports them."
  [:sequential :string])

(def Opts
  "An options map. Each lever documents the keys it reads and ignores the rest."
  [:map])

(def SubjectRef
  "A subject named as a qualified symbol, a var quote, or a var."
  :any)

(def Path
  "A filesystem path."
  :string)

(reg/register-all! {:hive.schemas/schema-ref  SchemaRef
                    :hive.schemas/violation   Violation
                    :hive.schemas/violations  Violations
                    :hive.schemas/opts        Opts
                    :hive.schemas/subject-ref SubjectRef
                    :hive.schemas/path        Path})
