(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Ran0Gen :> RANDOM_GEN where type RNG.Seed.t = Word32.t =
   MkRandomGen
      ((* <-- SML/NJ workarounds *)
       open TopLevel
       infixr 4 />
       (* SML/NJ workarounds --> *)
       structure Seed = Word32
       type t = Seed.t
       val ia : t = 0w16807
       val im : t = 0wx7FFFFFFF
       val iq : t = 0w127773
       val ir : t = 0w2836
       fun make s = case Seed.andb (s, im) of 0w0 => 0w12345678 | s => s
       fun value s = Seed.toWord (s - 0w1)
       fun next s = let
          val k = s div iq
          val s = ia * (s - k * iq) - ir * k
       in
          s + Seed.andb (Seed.~>> (s, 0w31), im)
       end
       fun split w = make o #2 o NumericalRecipes.psdes /> Seed.fromWord w
       val maxValue = Seed.toWord (im - 0w2))
