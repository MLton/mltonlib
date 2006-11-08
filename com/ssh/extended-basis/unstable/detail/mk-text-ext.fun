(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {TEXT} modules.
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
   structure CharArray =
      MkMonoArrayExt (structure MonoArray = CharArray
                      structure MonoVector = CharVector)

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
