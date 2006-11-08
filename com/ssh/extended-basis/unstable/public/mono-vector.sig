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

   val isoList : (vector, elem list) Iso.iso
   (**
    * An isomorphism between vectors and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   val isoPoly : (vector, elem Vector.vector) Iso.iso
   (**
    * An isomorphism between monomorphic and polymorphic vectors.  It is
    * always equivalent to {(toPoly, fromPoly)}.
    *)
end
