(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic hash function.
 *
 * Standard ML does not provide a means to extract the identity of a
 * mutable object as a hashable value.  This means that, regardless of
 * identity, two structurally equivalent mutable objects always hash to
 * the same word, which can degrade the asymptotic time complexity of
 * algorithms that hash mutable objects.
 *
 * Interestingly, hashing pure functions is possible.  More precisely, it
 * is possible to implement a non-trivial mapping - whose range is not a
 * singleton - of pure functions to words in such a way that equivalent
 * functions map to equal words.  This requires the ability to generate
 * values from the domains of functions.  However, it makes little sense
 * to provide such functionality solely for the purpose of hashing
 * functions, because it is impossible to compare functions for equality.
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
