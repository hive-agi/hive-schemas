(ns hive-schemas.schema
  "Unified entry point to the hive malli-schema levers.

   Re-exports the hive-spi.schema surface so downstream reaches for one
   namespace to register schemas and derive artifacts from them:

     register!/register-all!/deregister-all!/registered  registry mutation
     registry/schema/validate/explain                    registry access
     compile-op                                          schema -> op bundle
     input-schema                                        schema -> JSON Schema
     generator/generate/sample                           schema -> test.check gen
     ->type                                              schema -> Typed type"
  (:require [hive-spi.schema.registry :as reg]
            [hive-spi.schema.derive :as derive]
            [hive-spi.schema.gen :as gen]
            [hive-spi.schema.typed :as typed]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; --- registry ---
(def register!       reg/register!)
(def register-all!   reg/register-all!)
(def deregister-all! reg/deregister-all!)
(def registered      reg/registered)
(def registry        reg/registry)
(def schema          reg/schema)
(def validate        reg/validate)
(def explain         reg/explain)

;; --- derivation ---
(def compile-op   derive/compile-op)
(def input-schema derive/input-schema)

;; --- generation ---
(def generator gen/generator)
(def generate  gen/generate)
(def sample    gen/sample)

;; --- typing ---
(def ->type typed/schema->type)
