(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Order :> ORDER = struct
   open Order
   val swap = fn LESS    => GREATER
               | EQUAL   => EQUAL
               | GREATER => LESS
   fun isEqual   x = x = EQUAL
   fun isGreater x = x = GREATER
   fun isLess    x = x = LESS
   val orWhenEq = fn (EQUAL, th) => th ()
                   | (other,  _) => other
end
