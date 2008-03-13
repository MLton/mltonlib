(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {MONO_VECTOR} signature. *)
signature MONO_VECTOR = sig
   include BASIS_MONO_VECTOR

   type t sharing type t = vector
   (** Convenience alias. *)

   val empty : t Thunk.t
   (** Returns an empty vector. *)

   val unfoldi : (Int.t * 'a -> elem * 'a) -> Int.t * 'a -> t * 'a
   (**
    * {unfoldi f (n, b)} constructs a vector {v} of a length {n}, whose
    * elements {vi} are determined by the equations {b0 = b} and {(vi,
    * bi+1) = f (i, bi)}.
    *)

   (** == Conversions == *)

   val fromPoly : elem Vector.t -> t
   (**
    * Generates a monomorphic vector from the given polymorphic vector.
    * Specifically, the result of {fromPoly v} is equivalent to
    *
    *> tabulate (Vector.length v, fn i => Vector.sub (v, i))
    *)

   val toList : t -> elem List.t
   (**
    * Generates a list from the given vector.  Specifically, the result of
    * {toList v} is equivalent to {foldr op :: [] v}.
    *)

   val toPoly : t -> elem Vector.t
   (**
    * Generates a new polymorphic vector from the given monomorphic
    * vector.  Specifically, the result of {toPoly v} is equivalent to
    *
    *> Vector.tabulate (length v, fn i => Vector.sub (v, i))
    *)

   (** == Isomorphisms == *)

   val isoList : (t, elem List.t) Iso.t
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   val isoPoly : (t, elem Vector.t) Iso.t
   (**
    * An isomorphism between monomorphic and polymorphic vectors.  It is
    * always equivalent to {(toPoly, fromPoly)}.
    *)
end
