(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic type hash value.
 *
 * WARNING: The hash function is not designed to be secure in any way.
 *)
signature TYPE_HASH = sig
   structure TypeHashRep : OPEN_REP

   val typeHash : ('a, 'x) TypeHashRep.t -> Word32.t
   (** Returns a hash value specific to the type. *)
end

signature TYPE_HASH_CASES = sig
   include CASES TYPE_HASH
   sharing Open.Rep = TypeHashRep
end

signature WITH_TYPE_HASH_DOM = CASES
