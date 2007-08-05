(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithEq (Arg : OPEN_GENERIC) : EQ_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   fun seq length sub eq (l, r) = let
      val lL = length l
      val lR = length r
      fun lp i = let
         val i = i-1
      in
         i < 0 orelse eq (sub (l, i), sub (r, i))
                      andalso lp i
      end
   in
      lL = lR andalso lp lL
   end

   structure Eq =
      LayerGenericRep (structure Outer = Arg.Rep
                       structure Closed = MkClosedGenericRep (BinPr))

   open Eq.This

   val eq = getT
   fun notEq t = not o eq t
   fun withEq eq = mapT (const eq)

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Eq and Rep = Eq.Closed

      fun iso b (a2b, _) = BinPr.map a2b b
      val isoProduct = iso
      val isoSum     = iso

      val op *`  = Product.equal
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      val op +` = Sum.equal
      val unit  = op =
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Eq.--> unsupported"

      val exnHandler : Exn.t Rep.t Ref.t = ref GenericsUtil.failExnSq
      fun regExn t (_, e2to) =
          Ref.modify (fn exnHandler =>
                         fn (l, r) =>
                            case e2to l & e2to r of
                               NONE   & NONE   => exnHandler (l, r)
                             | SOME l & SOME r => t (l, r)
                             | _               => false) exnHandler
      fun exn ? = !exnHandler ?

      val list = ListPair.allEq

      fun vector ? = seq Vector.length Vector.sub ?

      fun array _ = op =
      fun refc  _ = op =

      val largeInt  = op =
      val largeReal = iso op = CastLargeReal.isoBits
      val largeWord = op =

      val bool   = op =
      val char   = op =
      val int    = op =
      val real   = iso op = CastReal.isoBits
      val string = op =
      val word   = op =

      val word8  = op =
      val word32 = op =
      val word64 = op =)

   open Layered
end
