(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure UnPr :> UN_PR = struct
   open UnPr
   fun op andAlso (p, q) ? = p ? andalso q ?
   fun op orElse (p, q) ? = p ? orelse q ?
   fun negate p = not o p
end
