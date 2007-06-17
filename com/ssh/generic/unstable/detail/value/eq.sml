(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithEq (Arg : OPEN_GENERIC) : EQ_GENERIC = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
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

      val op *` = Product.equal
      val op +` = Sum.equal

      val Y = Tie.function

      fun op --> _ = failing "Eq.--> unsupported"

      val exn : Exn.t Rep.t Ref.t = ref GenericsUtil.failExnSq
      fun regExn t (_, prj) =
          Ref.modify (fn exn =>
                         fn (l, r) =>
                            case prj l & prj r of
                               SOME l & SOME r => t (l, r)
                             | SOME _ & NONE   => false
                             | NONE   & SOME _ => false
                             | NONE   & NONE   => exn (l, r)) exn
      val exn = fn ? => !exn ?

      fun array _ = op =
      fun refc _ = op =

      val list = ListPair.allEq

      fun vector eq = iso (list eq) Vector.isoList (* XXX can be optimized *)

      val bool   = op =
      val char   = op =
      val int    = op =
      val real   = Real.==
      val string = op =
      val unit   = op =
      val word   = op =

      val largeInt  = op =
      val largeReal = LargeReal.==
      val largeWord = op =

      val word8  = op =
   (* val word16 = op = (* Word16 not provided by SML/NJ *) *)
      val word32 = op =
      val word64 = op =

      (* Trivialities *)

      val isoProduct = iso
      val isoSum = iso

      val T = id
      fun R _ = id
      val tuple = id
      val record = id

      fun C0 _ = unit
      fun C1 _ = id
      val data = id)

   open Layered
end
