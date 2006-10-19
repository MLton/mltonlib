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
            val intIso = (ord, chr)
            val minOrd = 0
            val boundsChar = (minChar, maxChar)
            val boundsOrd = (minOrd, maxOrd)
         end

      structure CharVector = MkMonoVectorExt (CharVector)
      structure CharArray =
         MkMonoArrayExt (structure MonoArray = CharArray
                         structure MonoVector = CharVector)

      structure String =
         struct
            open CharVector String
            val listIso = (explode, implode)
            val cStringEmb = (toCString, fromCString)
            val stringEmb = (toString, fromString)
         end
   end
