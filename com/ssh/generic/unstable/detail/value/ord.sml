(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithOrd (Arg : OPEN_CASES) : ORD_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 0 &
   (* SML/NJ workaround --> *)

   structure Ord = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (Cmp))

   open Ord.This

   val ord = getT
   fun withOrd cmp = mapT (const cmp)

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Ord and Rep = Ord.Closed

      fun iso b (a2b, _) = Cmp.map a2b b
      val isoProduct = iso
      val isoSum     = iso

      val op *`  = Product.collate
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      val op +` = Sum.collate
      val unit  = fn ((), ()) => EQUAL
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Ord.--> unsupported"

      val exns : (Exn.t Sq.t -> Order.t Option.t) Buffer.t = Buffer.new ()
      fun exn lr =
          recur 0 (fn lp =>
             fn i =>
                if i = Buffer.length exns
                then GenericsUtil.failExnSq lr
                else case Buffer.sub (exns, i) lr of
                        SOME r => r
                      | NONE   => lp (i+1))
      fun regExn cA (_, e2a) =
          (Buffer.push exns)
             (fn (l, r) =>
                 case e2a l & e2a r of
                    SOME l & SOME r => SOME (cA (l, r))
                  | SOME _ & NONE   => SOME GREATER
                  | NONE   & SOME _ => SOME LESS
                  | NONE   & NONE   => NONE)

      val array  = Array.collate
      val list   = List.collate
      val vector = Vector.collate

      fun refc t = Cmp.map ! t

      val largeInt  = LargeInt.compare
      val largeWord = LargeWord.compare
      val largeReal = iso CastLargeReal.Bits.compare CastLargeReal.isoBits

      val bool   = Bool.compare
      val char   = Char.compare
      val int    = Int.compare
      val real   = iso CastReal.Bits.compare CastReal.isoBits
      val string = String.compare
      val word   = Word.compare

      val word8  = Word8.compare
      val word32 = Word32.compare
      val word64 = Word64.compare)

   open Layered
end
