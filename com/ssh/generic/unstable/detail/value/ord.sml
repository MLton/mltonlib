(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithOrd (Arg : WITH_ORD_DOM) : ORD_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = (HashUniv.t, HashUniv.t) HashMap.t
   datatype 'a t = IN of e * 'a Sq.t -> Order.t

   fun lift (cmp : 'a Cmp.t) : 'a t = IN (cmp o #2)

   fun iso' (IN bX) (a2b, _) = IN (fn (e, bp) => bX (e, Sq.map a2b bp))

   val mkReal =
    fn Ops.R {isoBits = SOME isoBits, bitsOps = Ops.W {compare, ...}, ...} =>
       iso' (lift compare) isoBits
     | Ops.R {toBytes, ...} =>
       lift (Cmp.map toBytes (Word8Vector.collate Word8.compare))

   fun sequ (Ops.S {toSlice, getItem, ...}) (IN aO) =
       IN (fn (e, (l, r)) => let
                 fun lp (e, l, r) =
                     case getItem l & getItem r
                      of NONE        & NONE        => EQUAL
                       | NONE        & SOME _      => LESS
                       | SOME _      & NONE        => GREATER
                       | SOME (x, l) & SOME (y, r) =>
                         case aO (e, (x, y))
                          of EQUAL => lp (e, l, r)
                           | res   => res
              in
                 lp (e, toSlice l, toSlice r)
              end)

   fun cyclic aT (IN aO) =
       case HashUniv.new {eq = op =, hash = Word32.toWord o Arg.hash aT}
        of (to, _) =>
           IN (fn (e, (l, r)) => let
                     val lD = to l
                     val rD = to r
                  in
                     if case HashMap.find e lD
                         of SOME rD' => HashUniv.eq (rD, rD')
                          | NONE     => false
                     then EQUAL
                     else (HashMap.insert e (lD, rD)
                         ; HashMap.insert e (rD, lD)
                         ; aO (e, (l, r)))
                  end)

   val exns : (e * Exn.t Sq.t -> Order.t Option.t) Buffer.t = Buffer.new ()
   fun regExn (IN aO) (_, e2a) =
       (Buffer.push exns)
          (fn (e, (l, r)) =>
              case e2a l & e2a r
               of SOME l & SOME r => SOME (aO (e, (l, r)))
                | SOME _ & NONE   => SOME GREATER
                | NONE   & SOME _ => SOME LESS
                | NONE   & NONE   => NONE)

   structure OrdRep = LayerRep' (open Arg type 'a t = 'a t)

   open OrdRep.This

   fun ord t =
       case getT t
        of IN ord => fn xy =>
           ord (HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash}, xy)
   fun withOrd cmp = mapT (const (lift cmp))

   structure Open = LayerDepCases
     (fun iso        bT = iso' (getT bT)
      fun isoProduct bP = iso' (getP bP)
      fun isoSum     bS = iso' (getS bS)

      fun op *` (aP, bP) = let
         val IN aO = getP aP
         val IN bO = getP bP
      in
         IN (fn (e, (lA & lB, rA & rB)) =>
                case aO (e, (lA, rA))
                 of EQUAL => bO (e, (lB, rB))
                  | res   => res)
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val IN aO = getS aS
         val IN bO = getS bS
      in
         IN (fn (e, (l, r)) =>
                case l & r
                 of INL l & INL r => aO (e, (l, r))
                  | INL _ & INR _ => LESS
                  | INR _ & INL _ => GREATER
                  | INR l & INR r => bO (e, (l, r)))
      end
      val unit  = lift (fn ((), ()) => EQUAL)
      fun C0 _  = unit
      fun C1 _  = getT
      val data  = getS

      fun Y ? = let open Tie in iso function end (fn IN ? => ?, IN) ?

      fun op --> _ = IN (failing "Ord.--> unsupported")

      val exn = IN (fn (e, lr) =>
                       case Buffer.findSome (pass (e, lr)) exns
                        of NONE   => GenericsUtil.failExnSq lr
                         | SOME r => r)
      fun regExn0 _ = regExn unit
      fun regExn1 _ = regExn o getT

      fun array aT =
          cyclic (Arg.Open.array ignore aT) (sequ ArrayOps.ops (getT aT))
      fun list aT = sequ ListOps.ops (getT aT)
      fun vector aT = sequ VectorOps.ops (getT aT)

      fun refc aT = cyclic (Arg.Open.refc ignore aT) (iso aT (!, undefined))

      val fixedInt = lift FixedInt.compare
      val largeInt = lift LargeInt.compare

      val largeWord = lift LargeWord.compare
      val largeReal = mkReal LargeRealOps.ops
      val bool   = lift Bool.compare
      val char   = lift Char.compare
      val int    = lift Int.compare
      val real   = mkReal RealOps.ops
      val string = lift String.compare
      val word   = lift Word.compare

      val word8  = lift Word8.compare
      val word32 = lift Word32.compare
(*
      val word64 = lift Word64.compare
*)

      fun hole () = IN undefined

      open Arg OrdRep)
end
