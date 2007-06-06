(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RanQD1Gen :> RANDOM_GEN where type RNG.Seed.t = Word32.t =
   MkRandomGen
      ((* <-- SML/NJ workarounds *)
       open Fn
       infixr 4 />
       (* SML/NJ workarounds --> *)
       type t = Word32.t
       structure Seed = Word32
       val make = id
       val (value, seed) = Iso.<--> (Iso.swap Word.isoLarge, Word32.isoLarge)
       val next = NumericalRecipes.ranqd1
       fun split w = #2 o NumericalRecipes.psdes /> seed w
       val maxValue = value Word32.maxValue)
