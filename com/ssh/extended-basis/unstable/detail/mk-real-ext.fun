(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkRealExt (R : REAL) = struct
   open R
   type t = real
   val embDecimal = (toDecimal, fromDecimal)
   val embString = (toString, fromString)
   fun isoInt mode = (toInt mode, fromInt)
   fun isoLarge mode = (toLarge, fromLarge mode)
   fun isoLargeInt mode = (toLargeInt mode, fromLargeInt)
   val isoManExp = (toManExp, fromManExp)
end
