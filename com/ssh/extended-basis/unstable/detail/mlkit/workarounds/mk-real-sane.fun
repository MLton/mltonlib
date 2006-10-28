(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
