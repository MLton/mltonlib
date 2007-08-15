(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkIntegerExt (I : BASIS_INTEGER) : INTEGER = struct
   structure Core = struct
      open I
      type t = int
      type bounded = t
      type formattable = t
      type formattable_format = BasisStringCvt.radix
      type intable = t
      type largeable = t
      type largeable_large = BasisLargeInt.int
      type ordered = t
      type scannable = t
      type scannable_format = formattable_format
      type signed = t
      type stringable = t
      val zero = fromInt 0
      fun signBit (x:t) = x < zero
      fun copySign (x, y) = if sameSign (x, y) then x else ~x
      val embString = (toString, fromString)
      val isoInt = (toInt, fromInt)
      val isoLarge = (toLarge, fromLarge)
      val isoLargeInt as (toLargeInt, fromLargeInt) = isoLarge
      val isoFixedInt as (toFixedInt, fromFixedInt) =
          if case (precision, BasisInt.precision)
              of (SOME n, SOME m) => BasisInt.<= (n, m)
               | _                => false
          then (BasisFixedInt.fromInt o I.toInt,
                I.fromInt o BasisFixedInt.toInt)
          else (BasisFixedInt.fromLarge o I.toLarge,
                I.fromLarge o BasisFixedInt.toLarge)
      fun isZero i = zero = i
      fun isEven i = isZero (rem (i, fromInt 2))
      val isOdd = not o isEven
      val bounds =
          case (minInt, maxInt) of
             (NONE,         NONE) => NONE
           | (SOME min, SOME max) => SOME (min, max)
           | _                    => raise Fail "illegal"
   end

   structure MaybeBounded = MkMaybeBounded (Core)
   structure Ordered = MkOrdered (Core)
   structure Stringable = MkStringable (Core)

   open MaybeBounded
   open Ordered
   open Stringable

   open Core
end
