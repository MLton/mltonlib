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
   val decimalEmb = (toDecimal, fromDecimal)
   val intIso = (toInt IEEEReal.TO_NEAREST, fromInt)
   val largeIso = (toLarge, fromLarge IEEEReal.TO_NEAREST)
   val largeIntIso = (toLargeInt IEEEReal.TO_NEAREST, fromLargeInt)
   val manExpIso = (toManExp, fromManExp)
   val stringEmb = (toString, fromString)
end
