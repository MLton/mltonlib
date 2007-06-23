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

   structure Eq =
      LayerGenericRep (structure Outer = Arg.Rep
                       structure Closed = MkClosedGenericRep (BinPr))

   open Eq.This

   val eq = getT
   fun notEq ? = negate (getT ?)

   structure Layered = LayerGeneric
     (structure Outer = Arg and Result = Eq and Rep = Eq.Closed

      fun iso b (a2b, _) = b o Pair.map (Sq.mk a2b)
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

      val exn : Exn.t Rep.t Ref.t = ref GenericsUtil.failExnSq
      fun regExn t (_, e2to) =
          Ref.modify (fn exn =>
                         fn (l, r) =>
                            case e2to l & e2to r of
                               NONE   & NONE   => exn (l, r)
                             | NONE   & SOME _ => false
                             | SOME _ & NONE   => false
                             | SOME l & SOME r => t (l, r)) exn
      val exn = fn ? => !exn ?

      val list = ListPair.allEq

      fun seq length sub eq (l, r) = let
         val lL = length l
         val lR = length r
         fun lp i = i = lL
                    orelse eq (sub (l, i), sub (r, i))
                           andalso lp (i+1)
      in
         lL = lR andalso lp 0
      end

      fun vector ? = seq Vector.length Vector.sub ?
      fun array _ = op =

      fun refc _ = op =

      val largeInt  = op =
      val largeWord = op =

      val bool   = op =
      val char   = op =
      val int    = op =
      val string = op =
      val word   = op =

      fun mk cast = BinPr.map cast op =
      val largeReal = mk CastLargeReal.castToWord
      val      real = mk      CastReal.castToWord

      val word8  = op =
      val word32 = op =
      val word64 = op =)

   open Layered
end
