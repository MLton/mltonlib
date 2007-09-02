(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithOrd (Arg : WITH_ORD_DOM) : ORD_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 4 <\
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = (HashUniv.t, HashUniv.t) HashMap.t
   datatype r = LT | EQ of e | GT
   type 'a t = e * 'a Sq.t -> r

   fun lift (cmp : 'a Cmp.t) : 'a t =
    fn (e, xy) => case cmp xy
                   of EQUAL   => EQ e
                    | LESS    => LT
                    | GREATER => GT

   fun sequ {toSlice, getItem} aO (e, (l, r)) = let
      fun lp (e, l, r) =
          case getItem l & getItem r
           of NONE        & NONE        => EQ e
            | NONE        & SOME _      => LT
            | SOME _      & NONE        => GT
            | SOME (x, l) & SOME (y, r) =>
              case aO (e, (x, y))
               of EQ e => lp (e, l, r)
                | res  => res
   in
      lp (e, toSlice l, toSlice r)
   end

   fun cyclic aT aO = let
      val (to, _) = HashUniv.new {eq = op =, hash = Arg.hash aT}
   in
      fn (e, (l, r)) => let
            val lD = to l
            val rD = to r
         in
            if case HashMap.find e lD
                of SOME rD' => HashUniv.eq (rD, rD')
                 | NONE     => false
            then EQ e
            else (HashMap.insert e (lD, rD)
                ; HashMap.insert e (rD, lD)
                ; aO (e, (l, r)))
         end
   end

   val exns : (e * Exn.t Sq.t -> r Option.t) Buffer.t = Buffer.new ()
   fun regExn aO (_, e2a) =
       (Buffer.push exns)
          (fn (e, (l, r)) =>
              case e2a l & e2a r
               of SOME l & SOME r => SOME (aO (e, (l, r)))
                | SOME _ & NONE   => SOME GT
                | NONE   & SOME _ => SOME LT
                | NONE   & NONE   => NONE)

   fun iso' getX bX (a2b, _) (e, bp) = getX bX (e, Sq.map a2b bp)

   structure Ord = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Ord.This

   fun ord t = let
      val ord = getT t
   in
      fn xy =>
         case (ord (HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash}, xy))
          of LT => LESS | EQ _ => EQUAL | GT => GREATER
   end
   fun withOrd cmp = mapT (const (lift cmp))

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Ord

      fun iso        ? = iso' getT ?
      fun isoProduct ? = iso' getP ?
      fun isoSum     ? = iso' getS ?

      fun op *` (aP, bP) = let
         val aO = getP aP
         val bO = getP bP
      in
         fn (e, (lA & lB, rA & rB)) =>
            case aO (e, (lA, rA))
             of EQ e => bO (e, (lB, rB))
              | res  => res
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val aO = getS aS
         val bO = getS bS
      in
         fn (e, (l, r)) =>
            case l & r
             of INL l & INL r => aO (e, (l, r))
              | INL _ & INR _ => LT
              | INR _ & INL _ => GT
              | INR l & INR r => bO (e, (l, r))
      end
      val unit  = lift (fn ((), ()) => EQUAL)
      fun C0 _  = unit
      fun C1 _  = getT
      val data  = getS

      val Y = Tie.function

      fun op --> _ = failing "Ord.--> unsupported"

      fun exn (e, lr) =
          case Buffer.findSome (pass (e, lr)) exns
           of NONE   => GenericsUtil.failExnSq lr
            | SOME r => r
      fun regExn0 _ = regExn unit
      fun regExn1 _ = regExn o getT

      fun array aT = cyclic (Arg.array ignore aT)
                            (sequ {toSlice = ArraySlice.full,
                                   getItem = ArraySlice.getItem} (getT aT))
      fun list aT = sequ {toSlice = id, getItem = List.getItem} (getT aT)
      fun vector aT = sequ {toSlice = VectorSlice.full,
                            getItem = VectorSlice.getItem} (getT aT)

      fun refc aT = cyclic (Arg.refc ignore aT) (iso aT (!, undefined))

      val fixedInt = lift FixedInt.compare
      val largeInt = lift LargeInt.compare

      val largeWord = lift LargeWord.compare
      val largeReal =
          iso' id (lift CastLargeReal.Bits.compare) CastLargeReal.isoBits

      val bool   = lift Bool.compare
      val char   = lift Char.compare
      val int    = lift Int.compare
      val real   = iso' id (lift CastReal.Bits.compare) CastReal.isoBits
      val string = lift String.compare
      val word   = lift Word.compare

      val word8  = lift Word8.compare
      val word32 = lift Word32.compare
      val word64 = lift Word64.compare)

   open Layered
end
