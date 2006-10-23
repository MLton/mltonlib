(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {REAL} modules.
 *)
functor MkRealExt (R : REAL) = struct
   open R
   val embDecimal = (toDecimal, fromDecimal)
   val embString = (toString, fromString)
   fun isoInt mode = (toInt mode, fromInt)
   fun isoLarge mode = (toLarge, fromLarge mode)
   fun isoLargeInt mode = (toLargeInt mode, fromLargeInt)
   val isoManExp = (toManExp, fromManExp)
end
