(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithSeq (Arg : OPEN_CASES) : SEQ_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 4 <\
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = Univ.t List.t
   type 'a t = e * 'a Sq.t -> e * Bool.t

   fun lift (eq : 'a BinPr.t) : 'a t = Pair.map (id, eq)

   fun sequ {toSlice, getItem} aE (e, (l, r)) = let
      fun lp (e, l, r) =
          case getItem l & getItem r
           of NONE        & NONE        => (e, true)
            | NONE        & SOME _      => (e, false)
            | SOME _      & NONE        => (e, false)
            | SOME (x, l) & SOME (y, r) =>
              case aE (e, (x, y))
               of (e, true) => lp (e, l, r)
                | result    => result
   in
      lp (e, toSlice l, toSlice r)
   end

   fun cyclic t = let
      val (to, from) = Univ.Emb.new ()
      fun lp (e, [],    (l, r)) = t (to (l, r)::e, (l, r))
        | lp (e, u::us, (l, r)) =
          case from u
           of NONE        => lp (e, us, (l, r))
            | SOME (a, b) =>
              if a = l andalso b = r orelse a = r andalso b = l then
                 (e, true)
              else if (a = l) <> (b = r) orelse (a = r) <> (b = l) then
                 (e, false)
              else
                 lp (e, us, (l, r))
   in
      fn (e, (l, r)) => lp (e, e, (l, r))
   end

   val exns : (e * Exn.t Sq.t -> (e * Bool.t) Option.t) Buffer.t = Buffer.new ()
   fun regExn aE (_, e2a) =
       (Buffer.push exns)
          (fn (e, (l, r)) =>
              case e2a l & e2a r
               of SOME l & SOME r => SOME (aE (e, (l, r)))
                | SOME _ & NONE   => SOME (e, false)
                | NONE   & SOME _ => SOME (e, false)
                | NONE   & NONE   => NONE)

   structure Seq = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open Seq.This

   fun seq t = Pair.snd o [] <\ getT t
   fun notSeq t = negate (seq t)
   fun withSeq eq = mapT (const (lift eq))

   structure Layered = LayerCases
     (structure Outer = Arg and Result = Seq and Rep = Seq.Closed

      fun iso bE (a2b, _) (e, bp) = bE (e, Sq.map a2b bp)
      val isoProduct = iso
      val isoSum     = iso

      fun op *` (aE, bE) (e, (lA & lB, rA & rB)) =
          case aE (e, (lA, rA))
           of (e, true) => bE (e, (lB, rB))
            | result    => result
      val T      = id
      fun R _    = id
      val tuple  = id
      val record = id

      fun op +` (aE, bE) (e, (l, r)) =
          case l & r
           of INL l & INL r => aE (e, (l, r))
            | INL _ & INR _ => (e, false)
            | INR _ & INL _ => (e, false)
            | INR l & INR r => bE (e, (l, r))
      val unit  = lift (fn ((), ()) => true)
      fun C0 _  = unit
      fun C1 _  = id
      val data  = id

      val Y = Tie.function

      fun op --> _ = failing "Seq.--> unsupported"

      fun exn (e, lr) =
          case Buffer.findSome (pass (e, lr)) exns
           of NONE   => GenericsUtil.failExnSq lr
            | SOME r => r
      fun regExn0 _ (e, p) = regExn unit (const e, p)
      fun regExn1 _ = regExn

      fun array ? = cyclic (sequ {toSlice = ArraySlice.full,
                                  getItem = ArraySlice.getItem} ?)
      fun list ? = sequ {toSlice = id, getItem = List.getItem} ?
      fun vector ? = sequ {toSlice = VectorSlice.full,
                           getItem = VectorSlice.getItem} ?

      fun refc t = cyclic (iso t (!, undefined))

      val fixedInt = lift (op = : FixedInt.t BinPr.t)
      val largeInt = lift (op = : LargeInt.t BinPr.t)

      val largeWord = lift (op = : LargeWord.t BinPr.t)
      val largeReal = iso (lift op =) CastLargeReal.isoBits

      val bool   = lift (op = : Bool.t BinPr.t)
      val char   = lift (op = : Char.t BinPr.t)
      val int    = lift (op = : Int.t BinPr.t)
      val real   = iso (lift op =) CastReal.isoBits
      val string = lift (op = : String.t BinPr.t)
      val word   = lift (op = : Word.t BinPr.t)

      val word8  = lift (op = : Word8.t BinPr.t)
      val word32 = lift (op = : Word32.t BinPr.t)
      val word64 = lift (op = : Word64.t BinPr.t))

   open Layered
end
