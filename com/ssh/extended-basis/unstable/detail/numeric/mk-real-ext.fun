(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRealExt (R : BASIS_REAL) : REAL = struct
   open R
   type t = real
   fun sq x = x * x
   val embDecimal = (toDecimal, fromDecimal)
   val embString = (toString, fromString)
   fun isoInt mode = (toInt mode, fromInt)
   fun isoLarge mode = (toLarge, fromLarge mode)
   fun isoLargeInt mode = (toLargeInt mode, fromLargeInt)
   val isoManExp = (toManExp, fromManExp)
end
