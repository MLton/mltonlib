(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure RandomDev : RANDOM_DEV = struct
   (* XXX implement reasonable seed/useed for SML/NJ *)
   fun seed () = raise Fail "not implemented"
   val useed = seed
end
