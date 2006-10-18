(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {TEXT} modules.
 *)

functor MkTextExt (T : TEXT) =
   struct
      open T

      structure Char =
         struct
            open Char
            val int = (ord, chr)
            val minOrd = 0
            val boundsChar = (minChar, maxChar)
            val boundsOrd = (minOrd, maxOrd)
         end

      structure CharArray  = MkMonoArrayExt  (CharArray)
      structure CharVector = MkMonoVectorExt (CharVector)

      structure String =
         struct
            open CharVector String
            val list = (explode, implode)
            val cString = (toCString, fromCString)
            val string = (toString, fromString)
         end
   end
