(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure NumericalRecipes :> NUMERICAL_RECIPES = struct
   (* <-- SML/NJ workarounds *)
   open TopLevel
   infix  7 >> <<
   infix  6 andb
   infix  5 xorb
   infix  4 orb
   (* SML/NJ workarounds --> *)

   fun ranqd1 s : Word32.t = s * 0w1664525 + 0w1013904223

   val psdes =
       flip (foldl (fn ((c1, c2), (lw, rw)) => let
                       open Word32
                       val a = rw xorb c1
                       val al = a andb 0wxFFFF
                       val ah = a >> 0w16
                       val b = al*al + notb (ah*ah)
                    in (rw,
                        lw xorb (al*ah + (c2 xorb (b >> 0w16 orb b << 0w16))))
                    end))
            [(0wxBAA96887, 0wx4B0F3B58), (0wx1E17D32C, 0wxE874F0C3),
             (0wx03BCDC3C, 0wx6955C5A6), (0wx0F33D1B2, 0wx55A7CA46)]
end
