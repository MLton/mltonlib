(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RanQD1Gen :> RANDOM_GEN where type RNG.Seed.t = Word32.t =
   MkRandomGen
      ((* <-- SML/NJ workarounds *)
       open TopLevel
       infixr 4 />
       (* SML/NJ workarounds --> *)
       structure Seed = Word32
       type t = Seed.t
       val make = id
       val value = Seed.toWord
       val next = NumericalRecipes.ranqd1
       fun split w = #2 o NumericalRecipes.psdes /> Seed.fromWord w
       val maxValue = Seed.toWord Seed.maxValue)
