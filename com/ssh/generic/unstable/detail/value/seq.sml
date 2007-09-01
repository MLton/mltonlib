(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithSeq (Arg : WITH_SEQ_DOM) : SEQ_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 4 <\
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = (HashUniv.t, HashUniv.t) HashMap.t
   type 'a t = e * 'a Sq.t -> e Option.t

   fun lift (eq : 'a BinPr.t) : 'a t =
    fn (e, xy) => if eq xy then SOME e else NONE

   fun sequ {toSlice, getItem} aE (e, (l, r)) = let
      fun lp (e, l, r) =
          case getItem l & getItem r
           of NONE        & NONE        => SOME e
            | NONE        & SOME _      => NONE
            | SOME _      & NONE        => NONE
            | SOME (x, l) & SOME (y, r) =>
              case aE (e, (x, y))
               of SOME e => lp (e, l, r)
                | NONE   => NONE
   in
      lp (e, toSlice l, toSlice r)
   end

   fun cyclic aT aE = let
      val (to, _) = HashUniv.new {eq = op =, hash = Arg.hash aT}
   in
      fn (e, (l, r)) => let
            val lD = to l
            val rD = to r
         in
            case HashMap.find e lD
             of SOME rD' => if HashUniv.eq (rD, rD') then SOME e else NONE
              | NONE =>
                if isSome (HashMap.find e rD)
                then NONE
                else (HashMap.insert e (lD, rD)
                    ; HashMap.insert e (rD, lD)
                    ; aE (e, (l, r)))
         end
   end

   val exns : (e * Exn.t Sq.t -> e Option.t Option.t) Buffer.t = Buffer.new ()
   fun regExn aE (_, e2a) =
      (Buffer.push exns)
         (fn (e, (l, r)) =>
             case e2a l & e2a r
              of SOME l & SOME r => SOME (aE (e, (l, r)))
               | NONE   & NONE   => NONE
               | _               => SOME NONE)

   fun iso' getX bX =
       case getX bX
        of bE => fn (a2b, _) => fn (e, bp) => bE (e, Sq.map a2b bp)

   structure Seq = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Seq.This

   fun seq t =
       case getT t
        of eq => fn xy => isSome (eq (HashMap.new {eq = HashUniv.eq,
                                                   hash = HashUniv.hash}, xy))
   fun notSeq t = negate (seq t)
   fun withSeq eq = mapT (const (lift eq))

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Seq

      fun iso        ? = iso' getT ?
      fun isoProduct ? = iso' getP ?
      fun isoSum     ? = iso' getS ?

      fun op *` (aP, bP) = let
         val aE = getP aP
         val bE = getP bP
      in
         fn (e, (lA & lB, rA & rB)) =>
            case aE (e, (lA, rA))
             of SOME e => bE (e, (lB, rB))
              | NONE   => NONE
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val aE = getS aS
         val bE = getS bS
      in
         fn (e, (INL l, INL r)) => aE (e, (l, r))
          | (e, (INR l, INR r)) => bE (e, (l, r))
          | _                   => NONE
      end
      val unit  = lift (fn ((), ()) => true)
      fun C0 _  = unit
      fun C1 _  = getT
      val data  = getS

      val Y = Tie.function

      fun op --> _ = failing "Seq.--> unsupported"

      fun exn (e, lr) =
          case Buffer.findSome (pass (e, lr)) exns
           of NONE   => GenericsUtil.failExnSq lr
            | SOME r => r
      fun regExn0 _ (e, p) = regExn unit (const e, p)
      fun regExn1 _ = regExn o getT

      fun array aT =
          cyclic (Arg.array ignore aT)
                 (sequ {toSlice = ArraySlice.full,
                        getItem = ArraySlice.getItem} (getT aT))
      fun list aT = sequ {toSlice = id, getItem = List.getItem} (getT aT)
      fun vector aT = sequ {toSlice = VectorSlice.full,
                            getItem = VectorSlice.getItem} (getT aT)

      fun refc aT = cyclic (Arg.refc ignore aT) (iso aT (!, undefined))

      val fixedInt = lift (op = : FixedInt.t BinPr.t)
      val largeInt = lift (op = : LargeInt.t BinPr.t)

      val largeWord = lift (op = : LargeWord.t BinPr.t)
      val largeReal = iso' id (lift op =) CastLargeReal.isoBits

      val bool   = lift (op = : Bool.t BinPr.t)
      val char   = lift (op = : Char.t BinPr.t)
      val int    = lift (op = : Int.t BinPr.t)
      val real   = iso' id (lift op =) CastReal.isoBits
      val string = lift (op = : String.t BinPr.t)
      val word   = lift (op = : Word.t BinPr.t)

      val word8  = lift (op = : Word8.t BinPr.t)
      val word32 = lift (op = : Word32.t BinPr.t)
      val word64 = lift (op = : Word64.t BinPr.t))

   open Layered
end
