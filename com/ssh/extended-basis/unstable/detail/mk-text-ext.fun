(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkTextExt (T : TEXT) = struct
   open T

   structure Char = struct
      open Char
      type t = char
      val minOrd = 0
      val boundsChar = (minChar, maxChar)
      val boundsOrd = (minOrd, maxOrd)
      val isoInt = (ord, chr)
   end

   structure CharVector = MkMonoVectorExt (CharVector)

   structure CharVectorSlice =
      MkMonoVectorSliceExt (structure MonoVectorSlice = CharVectorSlice)

   structure CharArray =
      MkMonoArrayExt (structure MonoArray = CharArray
                      structure MonoVector = CharVector)

   structure CharArraySlice =
      MkMonoArraySliceExt (structure MonoArraySlice = CharArraySlice)

   structure String = struct
      open CharVector String
      type t = string
      val embCString = (toCString, fromCString)
      val embString = (toString, fromString)
      val isoList = (explode, implode)
   end

   structure Substring = struct
      open Substring
      type t = substring
      val length = size
   end
end
