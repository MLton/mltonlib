(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithOrd (Arg : OPEN_CASES) : ORD_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 4 <\
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = Univ.t List.t
   type 'a t = e * 'a Sq.t -> e * Order.t

   fun lift (cmp : 'a Cmp.t) : 'a t = Pair.map (id, cmp)

   fun seq {toSlice, getItem} aO (e, (l, r)) = let
      fun lp (e, l, r) =
          case (getItem l, getItem r)
           of (NONE       , NONE       ) => (e, EQUAL)
            | (NONE       , SOME _     ) => (e, LESS)
            | (SOME _     , NONE       ) => (e, GREATER)
            | (SOME (x, l), SOME (y, r)) =>
              case aO (e, (x, y))
               of (e, EQUAL) => lp (e, l, r)
                | result     => result
   in
      lp (e, toSlice l, toSlice r)
   end

   fun cyclic t = let
      val (to, from) = Univ.Emb.new ()
   in
      fn (e, (l, r)) =>
         if List.exists (fn u => case from u
                                  of NONE   => false
                                   | SOME p => p = (l, r) orelse p = (r, l)) e
         then (e, EQUAL)
         else t (to (l, r)::e, (l, r))
   end

   structure Ord = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Ord.This

   fun ord t = Pair.snd o [] <\ getT t
   fun withOrd cmp = mapT (const (lift cmp))

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Ord and Rep = Ord.Closed

      fun iso bO (a2b, _) (e, bp) = bO (e, Sq.map a2b bp)
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (aO, bO) (e, (lA & lB, rA & rB)) =
          case aO (e, (lA, rA))
           of (e, EQUAL) => bO (e, (lB, rB))
            | result     => result
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (aO, bO) (e, (l, r)) =
          case (l, r)
           of (INL l, INL r) => aO (e, (l, r))
            | (INL _, INR _) => (e, LESS)
            | (INR _, INL _) => (e, GREATER)
            | (INR l, INR r) => bO (e, (l, r))
      val unit  = lift (fn ((), ()) => EQUAL)
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Ord.--> unsupported"

      val exns : (e * Exn.t Sq.t -> (e * Order.t) Option.t) Buffer.t = Buffer.new ()
      fun exn (e, lr) =
          recur 0 (fn lp =>
             fn i =>
                if i = Buffer.length exns
                then GenericsUtil.failExnSq lr
                else case Buffer.sub (exns, i) (e, lr) of
                        SOME r => r
                      | NONE   => lp (i+1))
      fun regExn aO (_, e2a) =
          (Buffer.push exns)
             (fn (e, (l, r)) =>
                 case e2a l & e2a r of
                    SOME l & SOME r => SOME (aO (e, (l, r)))
                  | SOME _ & NONE   => SOME (e, GREATER)
                  | NONE   & SOME _ => SOME (e, LESS)
                  | NONE   & NONE   => NONE)

      fun array ? = cyclic (seq {toSlice = ArraySlice.full,
                                 getItem = ArraySlice.getItem} ?)
      fun list ? = seq {toSlice = id, getItem = List.getItem} ?
      fun vector ? = seq {toSlice = VectorSlice.full,
                          getItem = VectorSlice.getItem} ?

      fun refc t = cyclic (iso t (!, undefined))

      val fixedInt = lift FixedInt.compare
      val largeInt = lift LargeInt.compare

      val largeWord = lift LargeWord.compare
      val largeReal = iso (lift CastLargeReal.Bits.compare) CastLargeReal.isoBits

      val bool   = lift Bool.compare
      val char   = lift Char.compare
      val int    = lift Int.compare
      val real   = iso (lift CastReal.Bits.compare) CastReal.isoBits
      val string = lift String.compare
      val word   = lift Word.compare

      val word8  = lift Word8.compare
      val word32 = lift Word32.compare
      val word64 = lift Word64.compare)

   open Layered
end
