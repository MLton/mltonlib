(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkVec (Arg : VEC_CORE) : VEC = struct
   open Arg open Scalar Seq open Math

   type t = Scalar.t Seq.t
   val t = Seq.t Scalar.t

   val fromSeq = id
   val toSeq = id

   fun diag s v = map (fn f => update f (dup s, sub f v)) selector

   val e = diag (fromInt 0) (dup (fromInt 1))

   val ~| = map Scalar.~

   val zero = dup (fromInt 0)

   local
      fun mk f =
          case zipWith f
           of vv => vv & vv o Pair.map (id, dup) & vv o Pair.map (dup, id)
   in
      val op |+| & op |+ & op +| = mk op +
      val op |-| & op |- & op -| = mk op -
      val op |*| & op |* & op *| = mk op *
      val op |/| & op |/ & op /| = mk op /
   end

   local
      fun mk rel = all rel o zipWith id
   in
      val |<|  = mk op <
      val |<=| = mk op <=
      val |>|  = mk op >
      val |>=| = mk op >=
   end

   val minElems = zipWith min
   val maxElems = zipWith max

   val dot = sumWith op + o op |*|
   val norm = dot o Sq.mk
   val mag = sqrt o norm
   val invMag = fromInt 1 <\ op / o mag (* XXX invSqrt *)

   val eps = nextAfter (fromInt 1, fromInt 2) - fromInt 1

   fun normalize v = let
      val n = norm v
   in
      if n < eps then zero else v |* fromInt 1 / sqrt n (* XXX invSqrt *)
   end

   fun lerp (l, r) t = l |* (fromInt 1 - t) |+| r |* t
   fun nlerp (l, r) = normalize o lerp (l, r)
end
