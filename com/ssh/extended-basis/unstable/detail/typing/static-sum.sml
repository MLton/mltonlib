(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure StaticSum :> STATIC_SUM = struct
   type ('dL, 'cL, 'dR, 'cR, 'c) dom = ('dL -> 'cL) * ('dR -> 'cR)
   type ('dL, 'cL, 'dR, 'cR, 'c) cod = 'c
   type ('dL, 'cL, 'dR, 'cR, 'c) t =
        ('dL, 'cL, 'dR, 'cR, 'c) dom -> ('dL, 'cL, 'dR, 'cR, 'c) cod
   fun inL a (a2b, _) = a2b a
   fun inR c (_, c2d) = c2d c
   fun match x = x
   fun sum x f = f x
   fun split x = x (fn x => (inL x, inL x), fn x => (inR x, inR x))
   fun out x = x (match, match)
   fun map (f, g) = sum (inL o f, inR o g)
end
