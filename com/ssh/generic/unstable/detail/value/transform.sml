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
   datatype 'a t = IN of c * ('a * e -> 'a)

   fun lift f = f o Pair.fst

   val default = IN (ID, #1)

   fun un f2f (IN (c, f)) = if ID = c then default else IN (c, f2f f)
   fun bin fs2f (IN (aC, aT), IN (bC, bT)) =
       case join (aC, bC)
        of c => if ID = c then default else IN (c, fs2f (aT, bT))

   fun cyclic aT aF =
       case HashUniv.new {eq = op =, hash = Word32.toWord o Arg.hash aT}
        of (to, _) => fn (x, e) => case to x of xD =>
           if isSome (HashMap.find e xD) then x
           else (HashMap.insert e (xD, ()) ; aF (x, e))

   fun iso' bX (a2b, b2a) = un (Fn.map (Pair.map (a2b, id), b2a)) bX

   structure TransformRep = LayerRep' (open Arg type 'a t = 'a t)

   open TransformRep.This

   fun makeTransform t2u t a2a =
       case getT (t2u (mapT (const (IN (CUSTOM, lift a2a))) t))
        of IN (_, f) =>
           fn x => f (x, HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash})

   structure Open = LayerDepCases
     (fun iso        bT = iso' (getT bT)
      fun isoProduct bP = iso' (getP bP)
      fun isoSum     bS = iso' (getS bS)

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
                                (IN (REC, fn x => !r x),
                                 fn IN (c, f) =>
                                    if c <= REC
                                    then default
                                    else (r := f ; IN (CUSTOM, f)))
                             end) ?

      fun op --> _ = IN (ID, failing "Transform.--> has no default")

      val exn = default
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list aT = un (fn xF => fn (l, e) => map (xF /> e) l) (getT aT)

      fun vector aT =
          un (fn xF => fn (v, e) => Vector.map (xF /> e) v) (getT aT)

      fun array aT =
          un (fn xF => cyclic (Arg.Open.array ignore aT)
                              (fn (a, e) => (Array.modify (xF /> e) a ; a)))
             (getT aT)

      fun refc aT =
          un (fn xF => cyclic (Arg.Open.refc ignore aT)
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
(*
      val word64 = default
*)

      fun hole () = IN (CUSTOM, undefined)

      open Arg TransformRep)
end
