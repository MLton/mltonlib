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
   val isoInt = (toInt IEEEReal.TO_NEAREST, fromInt)
   val isoLarge = (toLarge, fromLarge IEEEReal.TO_NEAREST)
   val isoLargeInt = (toLargeInt IEEEReal.TO_NEAREST, fromLargeInt)
   val isoManExp = (toManExp, fromManExp)
end
