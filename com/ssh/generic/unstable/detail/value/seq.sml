(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithSeq (Arg : WITH_SEQ_DOM) : SEQ_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = (HashUniv.t, HashUniv.t) HashMap.t
   datatype 'a t = IN of e * 'a Sq.t -> Bool.t

   fun lift (eq : 'a BinPr.t) : 'a t = IN (eq o #2)

   fun sequ (Ops.S {toSlice, getItem, ...}) (IN aE) =
       IN (fn (e, (l, r)) => let
                 fun lp (e, l, r) =
                     case getItem l & getItem r
                      of NONE        & NONE        => true
                       | NONE        & SOME _      => false
                       | SOME _      & NONE        => false
                       | SOME (x, l) & SOME (y, r) =>
                         aE (e, (x, y)) andalso lp (e, l, r)
              in
                 lp (e, toSlice l, toSlice r)
              end)

   fun cyclic aT (IN aE) = let
      val (to, _) = HashUniv.new {eq = op =, hash = Word32.toWord o Arg.hash aT}
   in
      IN (fn (e, (l, r)) => let
                val lD = to l
                val rD = to r
             in
                case HashMap.find e lD
                 of SOME rD' => HashUniv.eq (rD, rD')
                  | NONE     => isNone (HashMap.find e rD)
                                andalso (HashMap.insert e (lD, rD)
                                       ; HashMap.insert e (rD, lD)
                                       ; aE (e, (l, r)))
             end)
   end

   val exns : (e * Exn.t Sq.t -> Bool.t Option.t) Buffer.t = Buffer.new ()
   fun regExn (IN aE) (_, e2a) =
       (Buffer.push exns)
          (fn (e, (l, r)) =>
              case e2a l & e2a r
               of SOME l & SOME r => SOME (aE (e, (l, r)))
                | NONE   & NONE   => NONE
                | _               => SOME false)

   fun iso' (IN bE) (a2b, _) = IN (fn (e, bp) => bE (e, Sq.map a2b bp))

   val mkReal =
    fn Ops.R {isoBits = SOME isoBits, ...} => iso' (lift op =) isoBits
     | Ops.R {toBytes, ...} => iso' (lift op =) (toBytes, undefined)

   structure SeqRep = LayerRep' (open Arg type 'a t = 'a t)

   open SeqRep.This

   fun seq t =
       case getT t
        of IN eq => fn xy =>
           eq (HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash}, xy)
   fun notSeq t = neg (seq t)
   fun withSeq eq = mapT (const (lift eq))

   structure Open = LayerDepCases
     (fun iso        bT = iso' (getT bT)
      fun isoProduct bP = iso' (getP bP)
      fun isoSum     bS = iso' (getS bS)

      fun op *` (aP, bP) = let
         val IN aE = getP aP
         val IN bE = getP bP
      in
         IN (fn (e, (lA & lB, rA & rB)) =>
                aE (e, (lA, rA)) andalso bE (e, (lB, rB)))
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val IN aE = getS aS
         val IN bE = getS bS
      in
         IN (fn (e, (INL l, INL r)) => aE (e, (l, r))
              | (e, (INR l, INR r)) => bE (e, (l, r))
              | _                   => false)
      end
      val unit  = lift (fn ((), ()) => true)
      fun C0 _  = unit
      fun C1 _  = getT
      val data  = getS

      fun Y ? = let open Tie in iso function end (fn IN ? => ?, IN) ?

      fun op --> _ = IN (failing "Seq.--> unsupported")

      val exn = IN (fn (e, lr) =>
                       case Buffer.findSome (pass (e, lr)) exns
                        of NONE   => GenericsUtil.failExnSq lr
                         | SOME r => r)
      fun regExn0 _ (e, p) = regExn unit (const e, p)
      fun regExn1 _ = regExn o getT

      fun array aT =
          cyclic (Arg.Open.array ignore aT) (sequ ArrayOps.ops (getT aT))
      fun list aT = sequ ListOps.ops (getT aT)
      fun vector aT = sequ VectorOps.ops (getT aT)

      fun refc aT = cyclic (Arg.Open.refc ignore aT) (iso aT (!, undefined))

      val fixedInt = lift op = : FixedInt.t t
      val largeInt = lift op = : LargeInt.t t

      val largeWord = lift op = : LargeWord.t t
      val largeReal = mkReal LargeRealOps.ops

      val bool   = lift op = : Bool.t t
      val char   = lift op = : Char.t t
      val int    = lift op = : Int.t t
      val real   = mkReal RealOps.ops
      val string = lift op = : String.t t
      val word   = lift op = : Word.t t

      val word8  = lift op = : Word8.t t
      val word32 = lift op = : Word32.t t
(*
      val word64 = lift op = : Word64.t t
*)

      fun hole () = IN undefined

      open Arg SeqRep)
end
