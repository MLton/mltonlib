(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnPr :> UN_PR = struct
   open UnPr
   fun map f p = p o f
   fun op andAlso (p, q) ? = p ? andalso q ?
   fun op orElse (p, q) ? = p ? orelse q ?
   fun neg p = not o p
end
