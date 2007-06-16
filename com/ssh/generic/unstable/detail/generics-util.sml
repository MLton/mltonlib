(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure GenericsUtil :> GENERICS_UTIL = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   val ` = Exn.name
   fun failCat ss = fail (concat ss)
   fun failExn e = failCat ["unregistered exn ", `e]
   fun failExnSq (l, r) = failCat ["unregistered exns ", `l, " and ", `r]

   fun op0 outer t x = outer (t, x)
   fun op1 outer f g = outer (Pair.map (f, g))
   fun op2 outer f g = outer (Pair.map (f, g) o Pair.swizzle)

   fun t outer t2p x2y = outer (Pair.map (t2p, x2y))
   fun r outer lt2p lx2y = outer (Pair.map o Pair.map (lt2p, lx2y) o Sq.mk)

   fun c0 outer l2s l2x = outer (Pair.map (l2s, l2x) o Sq.mk)
   fun c1 outer lt2s lx2y = outer (Pair.map o Pair.map (lt2s, lx2y) o Sq.mk)

   fun y outer x y = outer (Tie.tuple2 (x, y))

   fun morph outer iso' f = outer (fn (a, x) => fn i => (iso' a i, f x i))

   fun re outer ex ey =
       outer (fn (x, y) => fn e => (ex x e : Unit.t ; ey y e : Unit.t))
end

functor MkClosedGenericRep (type 'a t) : CLOSED_GENERIC_REP = struct
   type 'a t = 'a t
   type 'a s = 'a t
   type ('a, 'k) p = 'a t
end
