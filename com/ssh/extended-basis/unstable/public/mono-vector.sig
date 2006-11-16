(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_VECTOR} signature.
 *)
signature MONO_VECTOR = sig
   include MONO_VECTOR

   type t = vector
   (**
    * Convenience alias.
    *)

   val unfoldi : (int * 'a -> elem * 'a) -> int * 'a -> t * 'a
   (**
    * {unfoldi f (n, b)} constructs a vector {v} of a length {n}, whose
    * elements {vi} are determined by the equations {b0 = b} and {(vi,
    * bi+1) = f (i, bi)}.
    *)

   (** == Conversions == *)

   val fromPoly : elem Vector.vector -> vector
   (**
    * Generates a monomorphic vector from the given polymorphic vector.
    * Specifically, the result of {fromPoly v} is equivalent to
    *
    *> tabulate (Vector.length v, fn i => Vector.sub (v, i))
    *)

   val toList : vector -> elem list
   (**
    * Generates a list from the given vector.  Specifically, the result of
    * {toList v} is equivalent to {foldr op :: [] v}.
    *)

   val toPoly : vector -> elem Vector.vector
   (**
    * Generates a new polymorphic vector from the given monomorphic
    * vector.  Specifically, the result of {toPoly v} is equivalent to
    *
    *> Vector.tabulate (length v, fn i => Vector.sub (v, i))
    *)

   (** == Isomorphisms == *)

   val isoList : (vector, elem list) Iso.t
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   val isoPoly : (vector, elem Vector.vector) Iso.t
   (**
    * An isomorphism between monomorphic and polymorphic vectors.  It is
    * always equivalent to {(toPoly, fromPoly)}.
    *)
end
