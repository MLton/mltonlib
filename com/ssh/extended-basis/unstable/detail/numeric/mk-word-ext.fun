(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkWordExt (W : BASIS_WORD) : WORD = struct
   structure Core = struct
      open W
      type t = word
      type bitwise = t
      type bounded = t
      type formattable = t
      type formattable_format = BasisStringCvt.radix
      type intable = t
      type largeable = t
      type largeable_large = BasisLargeWord.word
      type ordered = t
      type scannable = t
      type scannable_format = formattable_format
      type shiftable = t
      type stringable = t
      type wordable = t
      val bounds = (fromInt 0, fromInt~1)
      val numBytes = BasisInt.quot (BasisInt.+ (wordSize, 7), 8)
      local
         fun mk fold bs =
             if numBytes <> BasisWord8Vector.length bs then
                raise Subscript
             else
                fold (fn (b, w) =>
                         W.orb (W.<< (w, 0w8),
                                W.fromLarge (BasisWord8.toLarge b)))
                     (W.fromInt 0)
                     bs
      in
         val fromBigBytes = mk BasisWord8Vector.foldl
         val fromLittleBytes = mk BasisWord8Vector.foldr
      end
      val fromWord = fromLarge o BasisWord.toLarge
      val fromWordX = fromLarge o BasisWord.toLargeX
      local
         fun mk idx w =
             BasisWord8Vector.tabulate
                (numBytes,
                 fn i =>
                    BasisWord8.fromLarge
                       (W.toLarge
                           (W.>> (w, BasisWord.*
                                        (0w8, BasisWord.fromInt (idx i))))))
      in
         val toBigBytes = mk (fn i => BasisInt.- (BasisInt.- (numBytes, 1), i))
         val toLittleBytes = mk (fn i => i)
      end
      val toWord = BasisWord.fromLarge o toLarge
      val toWordX = BasisWord.fromLarge o toLargeX
      val embString = (toString, fromString)
      val isoBigBytes = (toBigBytes, fromBigBytes)
      val isoInt = (toInt, fromInt)
      val isoIntX = (toIntX, fromInt)
      val isoLarge = (toLarge, fromLarge)
      val isoLargeX = (toLargeX, fromLarge)
      val isoLargeInt = (toLargeInt, fromLargeInt)
      val isoLargeIntX = (toLargeIntX, fromLargeInt)
      val isoLargeWord = isoLarge
      val isoLargeWordX = isoLargeX
      val isoLittleBytes = (toLittleBytes, fromLittleBytes)
      val isoWord = (toWord, fromWord)
      val isoWordX = (toWordX, fromWordX)
      fun isZero w = fromInt 0 = w
      fun isEven w = isZero (andb (fromInt 1, w))
      val isOdd = not o isEven
   end

   structure Bounded = MkBounded (Core)
   structure Ordered = MkOrdered (Core)
   structure Stringable = MkStringable (Core)

   open Bounded
   open Ordered
   open Stringable

   open Core
end
