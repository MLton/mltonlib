(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {REAL} modules.
 *)

functor MkRealExt (R : REAL) =
   struct
      open R
      val decimal = (toDecimal, fromDecimal)
      val int = (toInt IEEEReal.TO_NEAREST, fromInt)
      val large = (toLarge, fromLarge IEEEReal.TO_NEAREST)
      val largeInt = (toLargeInt IEEEReal.TO_NEAREST, fromLargeInt)
      val manExp = (toManExp, fromManExp)
      val string = (toString, fromString)
   end
