(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic hash function.
 *)
signature HASH = sig
   structure Hash : OPEN_GENERIC_REP

   val hash : ('a, 'x) Hash.t -> 'a -> Word.t
   (** Extracts the hash function. *)
end

signature HASH_GENERIC = sig
   include HASH OPEN_GENERIC
   sharing Hash = Rep
end
