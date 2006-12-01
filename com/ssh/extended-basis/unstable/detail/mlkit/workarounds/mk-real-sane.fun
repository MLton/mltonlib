(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRealSane (R : REAL) = struct
   open R
   local
      val unsupported = Fail "unsupported"
      val unsupported = fn _ => raise unsupported
   in
      val fromDecimal = unsupported
      val fromLarge = unsupported
      val fromLargeInt = unsupported
      val fromManExp = unsupported
      val toDecimal = unsupported
      val toInt = unsupported
      val toLarge = unsupported
      val toLargeInt = unsupported
      val toManExp = unsupported
   end
end
