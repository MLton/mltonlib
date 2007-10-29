(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRealSane (R : REAL) = struct
   open R
   val radix = 2
   val precision = 53
   fun fromDecimal _ = raise Fail "Real?.fromDecimal unsupported"
   fun fromLarge _ = raise Fail "Real?.fromLarge unsupported"
   fun fromLargeInt _ = raise Fail "Real?.fromLargeInt unsupported"
   fun fromManExp _ = raise Fail "Real?.fromManExp unsupported"
   fun toDecimal _ = raise Fail "Real?.toDecimal unsupported"
   fun toInt _ = raise Fail "Real?.toInt unsupported"
   fun toLarge _ = raise Fail "Real?.toLarge unsupported"
   fun toLargeInt _ = raise Fail "Real?.toLargeInt unsupported"
   fun toManExp _ = raise Fail "Real?.toManExp unsupported"
end
