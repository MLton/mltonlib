(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {MONO_ARRAY} signature. *)
signature MONO_ARRAY = sig
   include BASIS_MONO_ARRAY

   type t sharing type t = array
   (** Convenience alias. *)

   (** == Constructors == *)

   val empty : t Thunk.t
   (** Makes a new empty array. *)

   val duplicate : t UnOp.t
   (**
    * Makes a fresh duplicate of the given array.  {duplicate a} is
    * equivalent to {tabulate (length a, fn i => sub (a, i))}.
    *)

   (** == HOFs == *)

   val unfoldi : (Int.t * 'a -> elem * 'a) -> Int.t * 'a -> t * 'a
   (**
    * {unfoldi f (n, b)} constructs an array a of length {n}, whose
    * elements {ai} are determined by the equations {b0 = b} and {(ai,
    * bi+1) = f (i, bi)}.
    *)

   val map : elem UnOp.t -> t UnOp.t
   (** {map f} is equivalent to {fromVector o MonoVector.map f o toVector}. *)

   (** == Conversions == *)

   val fromPoly : elem Array.t -> t
   (**
    * Creates a new monomorphic array from the given polymorphic array.
    * Specifically, the expression {fromPoly a} is equivalent to the
    * expression
    *
    *> tabulate (Array.length a, fn i => Array.sub (a, i))
    *)

   val fromVector : vector -> t
   (**
    * Creates a new array from the given vector.  Specifically, the
    * expression {fromVector v} is equivalent to the expression
    *
    *> tabulate (Vector.length v, fn i => Vector.sub (v, i))
    *)

   val toList : t -> elem List.t
   (**
    * Generates a list from the given array.  Specifically, the result of
    * {toList a} is equivalent to {foldr op :: [] a}.
    *)

   val toPoly : t -> elem Array.t
   (**
    * Creates a new polymorphic array from the given monomorphic array.
    * Specifically, the expression {toPoly a} is equivalent to the
    * expression
    *
    *> Array.tabulate (length a, fn i => Array.sub (a, i))
    *)

   val toVector : t -> vector
   (**
    * Generates a vector from the given array.  Specifically, the result
    * of {toVector a} is equivalent to
    *
    *> MonoVector.tabulate (length a, fn i => sub (a, i))
    *
    * where {MonoVector} refers to the home structure of the monomorphic
    * {vector} type.
    *)

   (** == Isomorphisms == *)

   val isoList : (t, elem List.t) Iso.t
   (**
    * An isomorphism between arrays and lists.  It is always equivalent to
    * {(toList, fromList)}.  Note that the isomorphism does not preserve
    * identity.
    *)

   val isoPoly : (t, elem Array.t) Iso.t
   (**
    * An isomorphism between monomorphic and polymorphic arrays.  It is
    * always equivalent to {(toPoly, fromPoly)}.  Note that the
    * isomorphism does not preserve identity.
    *)

   val isoVector : (t, vector) Iso.t
   (**
    * An isomorphism between arrays and vectors.  It is always equivalent
    * to {(toVector, fromVector)}.  Note that the isomorphism does not
    * preserve identity.
    *)
end
