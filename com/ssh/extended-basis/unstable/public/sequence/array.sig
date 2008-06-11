(* Copyright (C) 2008 Vesa Karvonen
 * Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {ARRAY} signature. *)
signature ARRAY = sig
   include BASIS_ARRAY

   type 'a t = 'a array
   (** Convenience alias. *)

   (** == Constructors == *)

   val empty : 'a t Thunk.t
   (** Makes a new empty array. *)

   val duplicate : 'a t UnOp.t
   (**
    * Makes a fresh duplicate of the given array.  {duplicate a} is
    * equivalent to {tabulate (length a, fn i => sub (a, i))}.
    *)

   val for : 'a t -> 'a Effect.t Effect.t
   val fori : 'a t -> (Int.t * 'a) Effect.t Effect.t

   (** == HOFs == *)

   val unfoldi : (Int.t * 'b -> 'a * 'b) -> Int.t * 'b -> 'a t * 'b
   (**
    * {unfoldi f (n, b)} constructs an array a of length {n}, whose
    * elements {ai} are determined by the equations {b0 = b} and {(ai,
    * bi+1) = f (i, bi)}.
    *)

   val map : ('a -> 'b) -> 'a t -> 'b t
   (** {map f} is equivalent to {fromVector o Vector.map f o toVector}. *)

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
