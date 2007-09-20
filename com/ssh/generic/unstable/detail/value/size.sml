(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithSize (Arg : WITH_SIZE_DOM) : SIZE_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  0 &
   (* SML/NJ workaround --> *)

   type e = (HashUniv.t, Unit.t) HashMap.t

   datatype 'a t =
      STATIC  of Int.t
    | DYNAMIC of e * 'a -> Int.t

   val sz =
    fn STATIC  s => const s
     | DYNAMIC f => f

   fun bytes i = Word.toInt (Word.>> (Word.fromInt i + 0w7, 0w3))

   val wordSize = bytes Word.wordSize

   fun sequ length foldl =
    fn STATIC s  => (fn (_, a) => (s * length a + 2 * wordSize))
     | DYNAMIC f => (fn (e, a) =>
                        foldl (fn (x, s) => s + f (e, x)) (2 * wordSize) a)

   fun cyclic xT xS = let
      val (to, _) = HashUniv.new {eq = op =, hash = Arg.hash xT}
   in
      DYNAMIC (fn (e, x) => let
         val d = to x
      in
         case HashMap.find e d
          of SOME () => wordSize
           | NONE    => (HashMap.insert e (d, ()) ; xS (e, x))
      end)
   end

   fun intSize toLarge i =
       bytes (IntInf.log2 (abs (toLarge i) + 1))

   fun mkInt toLarge =
    fn SOME prec => STATIC (bytes prec)
     | NONE      => DYNAMIC (intSize toLarge o #2)

   fun mkWord wordSize = STATIC (bytes wordSize)

   val iso' =
    fn STATIC s   => const (STATIC s)
     | DYNAMIC bS => fn (a2b, _) => DYNAMIC (bS o Pair.map (id, a2b))

   structure SizeRep = LayerRep
     (structure Outer = Arg.Rep
      structure Closed = MkClosedRep (type 'a t = 'a t))

   open SizeRep.This

   fun staticSizeOf t =
       case getT t
        of STATIC s => SOME s
         | _        => NONE

   fun sizeOf t =
    case getT t
     of STATIC s  => const s
      | DYNAMIC f => fn x =>
        f (HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash} , x)

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = SizeRep

      fun iso        bT = iso' (getT bT)
      fun isoProduct bP = iso' (getP bP)
      fun isoSum     bS = iso' (getS bS)

      fun op *` (xP, yP) = let
         val xS = getP xP
         val yS = getP yP
      in
         case xS & yS
          of STATIC x & STATIC y => STATIC (x + y)
           | _                   =>
             DYNAMIC (fn (e, x & y) => sz xS (e, x) + sz yS (e, y))
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (xS, yS) = let
         val xS = getS xS
         val yS = getS yS
         val dyn =
             DYNAMIC (fn (e, INL x) => sz xS (e, x)
                       | (e, INR y) => sz yS (e, y))
      in
         case xS & yS
          of STATIC x & STATIC y => if x = y then STATIC x else dyn
           | _                   => dyn
      end

      val unit  = STATIC 0
      fun C0 _  = unit
      fun C1 _  = getT
      fun data xS = let
         val tagS = intSize Int.toLarge (Arg.numAlts xS)
      in
         case getS xS
          of STATIC s  => STATIC (tagS + s)
           | DYNAMIC f => DYNAMIC (fn ex => tagS + f ex)
      end

      fun Y ? = Tie.pure (fn () => let
         val r = ref (raising Fix.Fix)
         val f = DYNAMIC (fn ? => !r ?)
      in
         (f,
          fn DYNAMIC f' => (r := f' ; f)
           | STATIC s   => (r := const s ; STATIC s))
      end) ?

      fun op --> _ = DYNAMIC (failing "Size.--> unsupported")

      val exn : Exn.t t = DYNAMIC (failing "Size.exn not yet implemented")
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun list xT =
          case getT xT
           of STATIC c  => DYNAMIC (fn (_, xs) => (c + wordSize) * length xs)
            | DYNAMIC f =>
              DYNAMIC (fn (e, xs) => foldl (fn (x, s) => s + f (e, x)) 0 xs)

      fun vector xT = DYNAMIC (sequ Vector.length Vector.foldl (getT xT))

      fun array xT =
          cyclic (Arg.array ignore xT)
                 (sequ Array.length Array.foldl (getT xT))

      fun refc xT =
          cyclic (Arg.refc ignore xT)
                 (case getT xT
                   of STATIC s => const (s + wordSize)
                    | DYNAMIC f => fn (e, x) => wordSize + f (e, !x))

      val fixedInt = mkInt FixedInt.toLarge FixedInt.precision
      val largeInt = mkInt LargeInt.toLarge LargeInt.precision

      val largeReal = mkWord CastLargeReal.Bits.wordSize : LargeReal.t t
      val largeWord = mkWord LargeWord.wordSize : LargeWord.t t

      val bool   = STATIC 1
      val char   = STATIC 1
      val int    = mkInt Int.toLarge Int.precision
      val real   = mkWord CastReal.Bits.wordSize : Real.t t
      val string = DYNAMIC (fn (_, s) => size s + 2 * wordSize)
      val word   = mkWord Word.wordSize : Word.t t

      val word8  = mkWord  Word8.wordSize :  Word8.t t
      val word32 = mkWord Word32.wordSize : Word32.t t
      val word64 = mkWord Word64.wordSize : Word64.t t)

   open Layered
end
