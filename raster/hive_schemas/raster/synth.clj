(ns hive-schemas.raster.synth
  "OPTIONAL rung-0 facet: a raster `deftm` overload -> the hive-test trifecta.

   Needs BOTH source roots on the path: raster/ (the :raster alias) and synth/
   (the :test-synth alias).

   Levers:
     defkernel-trifecta  deftm var + opts -> the synthesized test vars"
  (:require [hive-schemas.raster :as hr]
            [hive-schemas.test :as hst]))

;; SPDX-License-Identifier: MIT
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn- subject-sym
  "Normalize a macro subject to its bare qualified symbol. Accepts `ns/fn` and
   the `#'ns/fn` reader form."
  [x]
  (if (and (seq? x) (= 'var (first x))) (second x) x))

(defmacro defkernel-trifecta
  "Synthesize `hive-schemas.test/deftrifecta-from-schema` for the ONE overload of
   deftm var `kernel` that `opts` selects. `kernel` is a bare `ns/fn` symbol or
   `#'ns/fn`.

   opts, beyond everything `deftrifecta-from-schema` accepts:
     :dtype     monomorphize a parametric deftm before schematizing
     :lengths   {length-param #{array-param ...}}, or :infer
     :max-len   largest generated array
     :magnitude bound on generated numbers
     :reference pure fn the kernel is differentially compared against
     :eps       tolerance for that comparison

   `:reference` becomes the `:rel` through `hive-schemas.raster/approx-rel`; an
   explicit `:rel` wins over it. Exact `=` is not offered: a compiled kernel
   disagrees with a reference in the last bits for honest reasons.

   Emits a `def` holding the resolved `{:in :out}` plus every facet var
   `deftrifecta-from-schema` emits."
  [name kernel opts]
  (let [subj      (subject-sym kernel)
        ssym      (gensym "kernel-schema-")
        sopts     (select-keys opts [:dtype :lengths :max-len :magnitude])
        rest-opts (dissoc opts :dtype :lengths :max-len :magnitude :reference :eps)
        rel-opt   (when (and (:reference opts) (not (:rel opts)))
                    {:rel `(hr/approx-rel ~(:reference opts) ~(when (:eps opts) {:eps (:eps opts)}))})]
    `(do
       (def ~ssym (hr/kernel-schema (var ~subj) ~sopts))
       (hst/deftrifecta-from-schema ~name ~subj
         ~(merge {:in (list :in ssym) :out (list :out ssym)} rel-opt rest-opts)))))
