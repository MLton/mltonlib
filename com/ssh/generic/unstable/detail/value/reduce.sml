(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithReduce (Arg : OPEN_GENERIC) : REDUCE_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   fun seq fold rA (c as {zero, +}) = let
      val rA = rA c
   in
      fold (fn (a, r) => rA a + r) zero
   end
       
   fun default {zero, + = _} = const zero

   structure Reduce = LayerGenericRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep
        (type 'a t = {zero : Univ.t, + : Univ.t BinOp.t} -> 'a -> Univ.t))

   fun makeReduce zero op + a2r tA tA2tB = let
      val (to, from) = Univ.Iso.new ()
      val c = {zero = to zero, + = BinOp.map (from, to) op +}
      val tA = Reduce.This.mapT (const (const (to o a2r))) tA
      val tB = tA2tB tA
   in
      from o Reduce.This.getT tB c
   end

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Reduce and Rep = Reduce.Closed

      fun iso rB (a2b, _) c = rB c o a2b
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (rA, rB) (c as {zero = _, +}) =
          op + o Pair.map (rA c, rB c) o Product.toTuple2
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (rA, rB) c = Sum.sum (rA c, rB c)
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Reduce.--> has no default"

      fun regExn _ _ = ()
      fun exn _ = fail "Reduce.exn not yet implemented"

      fun list   ? = seq   List.foldl ?
      fun vector ? = seq Vector.foldl ?
      fun array  ? = seq  Array.foldl ?

      fun refc rA c = rA c o !

      val largeInt  = default
      val largeReal = default
      val largeWord = default

      val bool   = default
      val char   = default
      val int    = default
      val real   = default
      val string = default
      val word   = default

      val word8  = default
      val word32 = default
      val word64 = default)

   open Layered
end
