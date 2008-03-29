(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkTextExt (structure Text : BASIS_TEXT
                   val stringToBytes :
                       Text.String.string -> BasisWord8Vector.vector
                   val bytesToString :
                       BasisWord8Vector.vector -> Text.String.string
                   val ch_0 : Text.Char.char val ch_1 : Text.Char.char
                   val ch_7 : Text.Char.char val ch_9 : Text.Char.char
                   val ch_a : Text.Char.char val ch_f : Text.Char.char
                   val ch_A : Text.Char.char val ch_F : Text.Char.char) : TEXT =
struct
   open Text

   structure Char : CHAR = struct
      structure Core = struct
         open Char
         type t = char
         type bounded = t
         type cased = t
         type cstringable = t
         type ordered = t
         type scannable = t
         type stringable = t
         val boundsChar = (minChar, maxChar)
         val bounds = boundsChar
         val minOrd = 0
         val boundsOrd = (minOrd, maxOrd)
         val isoInt = (ord, chr)
         val embCString = (toCString, fromCString)
         val embString = (toString, fromString)
      end

      structure Bounded = MkBounded (Core)
      structure CStringable = MkCStringable (Core)
      structure Ordered = MkOrdered (Core)
      structure Stringable = MkStringable (Core)

      open Bounded
      open CStringable
      open Ordered
      open Stringable

      open Core

      val isBinDigit = inRange (ch_0, ch_1)
      val isOctDigit = inRange (ch_0, ch_7)

      fun domain b = if b then () else raise Domain

      local
         fun dig i = chr (i + ord ch_0)
         fun mk m =
             (fn c => (domain (inRange (ch_0, dig m) c) ; ord c - ord ch_0),
              fn i => (domain (Int.inRange (0, m) i) ; dig i))
      in
         val binDigitIsoInt as (binDigitToInt, intToBinDigit) = mk 1
         val octDigitIsoInt as (octDigitToInt, intToOctDigit) = mk 7
         val digitIsoInt    as (digitToInt,    intToDigit)    = mk 9
      end

      val hexDigitIsoInt as (hexDigitToInt, intToHexDigit) =
          (fn c => ord c - (if inRange (ch_0, ch_9) c
                            then ord ch_0
                            else if inRange (ch_a, ch_f) c
                            then ord ch_a - 10
                            else (domain (inRange (ch_A, ch_F) c)
                                ; ord ch_A - 10)),
           fn i => chr (i + (if Int.inRange (0, 9) i
                             then ord ch_0
                             else (domain (Int.inRange (10, 15) i)
                                 ; ord ch_A - 10))))
   end

   structure CharVector = MkMonoVectorExt (CharVector)

   structure CharVectorSlice =
      MkMonoVectorSliceExt (structure MonoVectorSlice = CharVectorSlice)

   structure CharArray =
      MkMonoArrayExt (structure MonoArray = CharArray
                      structure MonoVector = CharVector)

   structure CharArraySlice =
      MkMonoArraySliceExt (structure MonoArraySlice = CharArraySlice)

   structure String : STRING = struct
      structure Core = struct
         open CharVector
         open String
         type t = string
         type cased = t
         type cstringable = t
         type ordered = t
         type scannable = t
         type stringable = t
         val toBytes = stringToBytes
         val fromBytes = bytesToString
         val isoBytes = (toBytes, fromBytes)
         val toUpper = map Char.toUpper
         val toLower = map Char.toLower
         val embCString = (toCString, fromCString)
         val embString = (toString, fromString)
         val isoList = (explode, implode)
      end

      structure CStringable = MkCStringable (Core)
      structure Ordered = MkOrdered (Core)
      structure Scannable = MkScannable (Core)
      structure Stringable = MkStringable (Core)

      open CStringable
      open Ordered
      open Scannable
      open Stringable

      open Core
   end

   structure Substring : SUBSTRING = struct
      open Substring
      type t = substring
      val length = size
      fun droplr p = dropl p o dropr p
      fun extendl pred ss = let
         val (s, i, n) = base ss
         val j = i+n
         fun lp i = if 0 < i andalso pred (String.sub (s, i-1)) then lp (i-1)
                    else substring (s, i, j-i)
      in
         lp i
      end
      fun extendr pred ss = let
         val (s, i, n) = base ss
         val m = String.size s
         fun lp j = if j < m andalso pred (String.sub (s, j)) then lp (j+1)
                    else substring (s, i, j-i)
      in
         lp (i+n)
      end
   end
end
