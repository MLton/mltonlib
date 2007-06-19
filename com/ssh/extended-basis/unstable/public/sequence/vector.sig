(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {VECTOR} signature. *)
signature VECTOR = sig
   include BASIS_VECTOR

   type 'a t = 'a vector
   (** Convenience alias. *)

   val empty : 'a t Thunk.t
   (** Returns an empty vector. *)

   val unfoldi : (Int.t * 'b -> 'a * 'b) -> Int.t * 'b -> 'a t * 'b
   (**
    * {unfoldi f (n, b)} constructs a vector {v} of a length {n}, whose
    * elements {vi} are determined by the equations {b0 = b} and {(vi,
    * bi+1) = f (i, bi)}.
    *)

   (** == Conversions == *)

   val toList : 'a t -> 'a List.t
   (**
    * Generates a list from the given vector.  Specifically, the result of
    * {toList v} is equivalent to {foldr op :: [] v}.
    *)

   (** == Isomorphisms == *)

   val isoList : ('a t, 'a List.t) Iso.t
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)
end
