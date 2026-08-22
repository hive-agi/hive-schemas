(ns ^:typed.clojure hive-schemas.raster-tc-fixture
  "A namespace that type-checks ONLY when raster's Typed Clojure extensions are
   loaded. `(* 32 64)` is `t/Int` to the stock checker and `(t/Val 2048)` once
   `raster.compiler.core.tc-extensions` has registered its value-propagating
   `-invoke-special` handlers, so the annotation below is the discriminator.

   Not a test namespace — the suite checks it, it does not run."
  (:require [clojure.core.typed :as t]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(t/ann shape-product (t/Val 2048))
(def shape-product (* 32 64))
