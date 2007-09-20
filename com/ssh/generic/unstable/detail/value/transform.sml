(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithTransform (Arg : WITH_TRANSFORM_DOM) : TRANSFORM_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infixr 4 />
   infix  0 &
   (* SML/NJ workaround --> *)

   type c = Word.t
   val ID = 0w0 and REC = 0w1 and CUSTOM = 0w2
   val join = Word.orb

   type e = (HashUniv.t, Unit.t) HashMap.t
   type 'a t = c * ('a * e -> 'a)

   fun lift f = f o Pair.fst

   val default : 'a t = (ID, #1)

   fun un f2f (c, f) = if ID = c then default else (c, f2f f)
   fun bin fs2f ((aC, aT), (bC, bT)) =
       case join (aC, bC) of c => if ID = c then default else (c, fs2f (aT, bT))

   fun cyclic aT aF =
       case HashUniv.new {eq = op =, hash = Arg.hash aT}
        of (to, _) =>
           fn (x, e) =>
              case to x of xD => if isSome (HashMap.find e xD) then x
                                 else (HashMap.insert e (xD, ()) ; aF (x, e))

   fun iso' getX bX (a2b, b2a) = un (Fn.map (Pair.map (a2b, id), b2a)) (getX bX)

   structure TransformRep = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open TransformRep.This

   fun makeTransform a2a t t2u =
       case getT (t2u (mapT (const (CUSTOM, lift a2a)) t))
        of (_, f) =>
           fn x => f (x, HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash})

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = TransformRep

      fun iso        ? = iso' getT ?
      fun isoProduct ? = iso' getP ?
      fun isoSum     ? = iso' getS ?

      fun op *` (aP, bP) =
          bin (fn (aT, bT) => fn (a & b, e) => aT (a, e) & bT (b, e))
              (getP aP, getP bP)
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) =
          bin (fn (aT, bT) => fn (INL a, e) => INL (aT (a, e))
                               | (INR b, e) => INR (bT (b, e)))
              (getS aS, getS bS)
      val unit  = default
      fun C0 _  = unit
      fun C1 _  = getT
      val data  = getS

      fun Y ? = Tie.pure (fn () => let
                                val r = ref (raising Fix.Fix)
                             in
                                ((REC, fn x => !r x),
                                 fn (c, f) =>
                                    if c <= REC
                                    then default
                                    else (r := f ; (CUSTOM, f)))
                             end) ?

      fun op --> _ = (ID, failing "Transform.--> has no default")

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list aT = un (fn xF => fn (l, e) => map (xF /> e) l) (getT aT)

      fun vector aT = un (fn xF => fn (v, e) => Vector.map (xF /> e) v) (getT aT)

      fun array aT =
          un (fn xF => cyclic (Arg.array ignore aT)
                              (fn (a, e) => (Array.modify (xF /> e) a ; a)))
             (getT aT)

      fun refc aT =
          un (fn xF => cyclic (Arg.refc ignore aT)
                              (fn (r, e) => (r := xF (!r, e) ; r)))
             (getT aT)

      val fixedInt = default
      val largeInt = default

      val largeReal = default
      val largeWord = default

      val bool   = default
      val char   = default
      val int    = default
      val real   = default
      val string = default
      val word   = default

      val word8  = default
      val word32 = default
      val word64 = default)

   open Layered
end
