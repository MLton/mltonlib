(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithEnum (Arg : WITH_ENUM_DOM) = let
   structure Result = struct
      (* <-- SML/NJ workaround *)
      open TopLevel
      infix  4 <\
      infix  0 &
      (* SML/NJ workaround --> *)

      infixr :::

      structure Enum = struct
         datatype 'a t = IN of Unit.t -> ('a * 'a t) Option.t
         fun get (IN t) = t ()
         val empty = IN (fn () => NONE)
(*
         fun takeAtMost (e, n) =
             IN (fn () =>
                    if n <= 0
                    then NONE
                    else case get e
                          of NONE        => NONE
                           | SOME (x, e) => SOME (x, takeAtMost (e, n-1)))
         fun toList e = let
            fun lp (xs, e) =
                case get e
                 of NONE        => rev xs
                  | SOME (x, e) => lp (x::xs, e)
         in
            lp ([], e)
         end
*)
         fun interleave (xs, ys) =
             IN (fn () =>
                    case get xs
                     of NONE         => get ys
                      | SOME (x, xs) => SOME (x, interleave (ys, xs)))
(*
         fun iterate f x =
             IN (fn () => SOME (x, iterate f (f x)))
*)
         fun iterateUnless f x =
             IN (fn () => SOME (x, iterateUnless f (f x) handle _ => empty))
         fun map f xs =
             IN (fn () =>
                    case get xs
                     of NONE         => NONE
                      | SOME (x, xs) => SOME (f x, map f xs))
         fun nonEmptyTails xs =
             IN (fn () =>
                    case get xs
                     of NONE          => NONE
                      | SOME (_, xs') => SOME (xs, nonEmptyTails xs'))
         fun x ::: xs = IN (fn () => SOME (x, xs))
      end

      open Enum

      fun iso' b (_, b2a) = map b2a b

      fun product (xs, ys) = let
         fun lp zss =
             IN (fn () =>
                    case get zss
                     of NONE           => NONE
                      | SOME (zs, zss) => get (interleave (zs, lp zss)))
      in
         lp (map (fn xs => map (fn y => #1 (valOf (get xs)) & y) ys)
                 (nonEmptyTails xs))
      end

      fun list' a =
          IN (fn () => get (interleave ([]:::empty,
                                        map (fn x & xs => x::xs)
                                            (product (a, list' a)))))

      fun mkInt zero one ~ op + =
          interleave (iterateUnless ( one <\ op +) zero,
                      iterateUnless (~one <\ op +) (~one))

      fun mkWord one op + (min, max) =
          iterateUnless (fn w => if w = max then raise Overflow else w + one)
                        min

      fun mkReal zero posInf ~ nextAfter =
          interleave (iterateUnless (fn r => nextAfter (r,  posInf)) zero,
                      iterateUnless (fn r => nextAfter (r, ~posInf)) (~zero))

      structure EnumRep = LayerRep' (open Arg Enum)

      open EnumRep.This

      val enum = getT

      structure Open = LayerDepCases
        (fun iso        bT = iso' (getT bT)
         fun isoProduct bP = iso' (getP bP)
         fun isoSum     bS = iso' (getS bS)

         fun op *` (xs, ys) = product (getP xs, getP ys) 
         val T      = getT
         fun R _    = getT
         val tuple  = getP
         val record = getP

         fun op +` (aS, bS) = let
            val a = map INL (getS aS)
            val b = map INR (getS bS)
         in
            interleave (if Arg.hasBaseCase aS then (a, b) else (b, a))
         end
         val unit  = ():::empty
         fun C0 _  = unit
         fun C1 _  = getT
         val data  = getS

         fun Y ? = Tie.iso Tie.function (fn IN ? => ?, IN) ?

         fun op --> _ = empty (* XXX: not yet implemented *)

         val exn = empty (* XXX: not yet implemented *)
         fun regExn0 _ _ = ()
         fun regExn1 _ _ _ = ()

         fun list a = list' (getT a)
         fun vector a = iso' (list a) Vector.isoList

         fun array a = iso' (list a) Array.isoList
         fun refc a = iso a (undefined, ref)

         val fixedInt = mkInt 0 1 ~ FixedInt.+
         val largeInt = mkInt 0 1 ~ LargeInt.+

         val largeReal = mkReal 0.0 LargeReal.posInf ~ LargeReal.nextAfter
         val largeWord = mkWord 0w1 op + LargeWord.bounds

         val bool = false:::true:::empty
         val char = iterateUnless (chr o 1 <\ op + o ord) Char.minValue
         val int = mkInt 0 1 ~ Int.+
         val real = mkReal 0.0 Real.posInf ~ Real.nextAfter
         val string = iso' (list' char) String.isoList
         val word = mkWord 0w1 op + Word.bounds

         val word8 = mkWord 0w1 op + Word8.bounds
         val word32 = mkWord 0w1 op + Word32.bounds
(*
         val word64 = mkWord 0w1 op + Word64.bounds
*)

         fun hole () = IN undefined

         open Arg EnumRep)
   end
in
   Result :> ENUM_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end
