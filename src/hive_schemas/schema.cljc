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
            [hive-spi.schema.typed :as typed]
            [malli.core :as m]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

;; Every fn re-export DELEGATES through the owning var at call time instead of
;; `(def x owner/x)`, which captures the fn OBJECT: a reload of hive-spi (a fix
;; applied in a live image) would never reach callers entering here. See epic
;; 20260821211613-0c0e618f. `registry` is a malli registry VALUE, not a fn, so
;; it stays a plain def.

;; --- registry ---
(defn register!       [& args] (apply reg/register! args))
(defn register-all!   [& args] (apply reg/register-all! args))
(defn deregister-all! [& args] (apply reg/deregister-all! args))
(defn registered      [& args] (apply reg/registered args))
(def registry        reg/registry)
(defn schema          [& args] (apply reg/schema args))
(defn validate        [& args] (apply reg/validate args))
(defn explain         [& args] (apply reg/explain args))

;; --- derivation ---
(defn compile-op   [& args] (apply derive/compile-op args))
(defn input-schema [& args] (apply derive/input-schema args))

;; --- generation ---
(defn generator [& args] (apply gen/generator args))
(defn generate  [& args] (apply gen/generate args))
(defn sample    [& args] (apply gen/sample args))

;; --- typing ---
(defn ->type [& args] (apply typed/schema->type args))

;; --- contracts: mirror hive-spi's own m/=> on each owner fn ---
(m/=> register!       [:=> [:cat :qualified-keyword :any] :qualified-keyword])
(m/=> register-all!   [:=> [:cat [:map-of :qualified-keyword :any]] [:vector :qualified-keyword]])
(m/=> deregister-all! [:=> [:cat [:or [:sequential :keyword] [:set :keyword]]] [:vector :keyword]])
(m/=> registered      [:=> [:cat] [:map-of :keyword :any]])
(m/=> schema          [:=> [:cat reg/SchemaRef] reg/CompiledSchema])
(m/=> validate        [:=> [:cat reg/SchemaRef :any] :boolean])
(m/=> explain         [:=> [:cat reg/SchemaRef :any] [:maybe :map]])
(m/=> compile-op      [:=> [:cat reg/SchemaRef] derive/OpBundle])
(m/=> input-schema    [:=> [:cat reg/SchemaRef] :map])
(m/=> generator [:function
                 [:=> [:cat reg/SchemaRef] gen/Generator]
                 [:=> [:cat reg/SchemaRef gen/GenOpts] gen/Generator]])
(m/=> generate [:function
                [:=> [:cat reg/SchemaRef] :any]
                [:=> [:cat reg/SchemaRef gen/GenOpts] :any]])
(m/=> sample [:function
              [:=> [:cat reg/SchemaRef] [:sequential :any]]
              [:=> [:cat reg/SchemaRef gen/GenOpts] [:sequential :any]]])
(m/=> ->type [:=> [:cat reg/SchemaRef] typed/TypeSyntax])
