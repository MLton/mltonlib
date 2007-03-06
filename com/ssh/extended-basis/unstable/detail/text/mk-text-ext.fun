(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkTextExt (T : BASIS_TEXT) : TEXT = struct
   open T

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
   end
end
