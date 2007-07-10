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

   val hashParam :
       ('a, 'x) Hash.t -> {totWidth : Int.t, maxDepth : Int.t} -> 'a -> Word.t
   (**
    * Returns a hash function.  The {totWidth} and {maxDepth}
    * parameters give some control over hashing.  The {totWidth}
    * parameter controls how many elements of sequences, like lists
    * and vectors, will be examined.  The {maxDepth} parameter
    * controls how many times the hash function descends into a
    * (possibly recursive) datatype.
    *)

   val hash : ('a, 'x) Hash.t -> 'a -> Word.t
   (** Returns the default hash function. *)
end

signature HASH_GENERIC = sig
   include OPEN_GENERIC HASH
   sharing Rep = Hash
end

signature WITH_HASH_DOM = TYPE_INFO_GENERIC
