(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Extended {ARRAY} signature. *)
signature ARRAY = sig
   include ARRAY

   type 'a t = 'a array
   (** Convenience alias. *)

   val duplicate : 'a t -> 'a t
   (**
    * Makes a fresh duplicate of the given array.  {duplicate a} is
    * equivalent to {tabulate (length a, fn i => sub (a, i))}.
    *)

   val unfoldi : (Int.t * 'b -> 'a * 'b) -> Int.t * 'b -> 'a t * 'b
   (**
    * {unfoldi f (n, b)} constructs an array a of length {n}, whose
    * elements {ai} are determined by the equations {b0 = b} and {(ai,
    * bi+1) = f (i, bi)}.
    *)

   (** == Conversions == *)

   val fromVector : 'a Vector.t -> 'a t
   (**
    * Creates a new array from the given vector.  Specifically, the
    * expression {fromVector v} is equivalent to the expression
    *
    *> tabulate (Vector.length v, fn i => Vector.sub (v, i))
    *)

   val toList : 'a t -> 'a List.t
   (**
    * Generates a list from the given array.  Specifically, the result of
    * {toList a} is equivalent to {foldr op :: [] a}.
    *)

   val toVector : 'a t -> 'a Vector.t
   (**
    * Generates a vector from the given array.  Specifically, the result
    * of {toVector a} is equivalent to
    *
    *> Vector.tabulate (length a, fn i => sub (a, i))
    *)

   (** == Isomorphisms == *)

   val isoList : ('a t, 'a List.t) Iso.t
   (**
    * An isomorphism between arrays and lists.  It is always equivalent to
    * {(toList, fromList)}.  Note that the isomorphism does not preserve
    * identity.
    *)

   val isoVector : ('a t, 'a Vector.t) Iso.t
   (**
    * An isomorphism between arrays and vectors.  It is always equivalent
    * to {(toVector, fromVector)}.  Note that the isomorphism does not
    * preserve identity.
    *)
end
