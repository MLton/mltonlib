(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_ARRAY} signature.
 *)
signature MONO_ARRAY = sig
   include MONO_ARRAY

   (** == Conversions == *)

   val fromPoly : elem Array.array -> array
   (**
    * Creates a new monomorphic array from the given polymorphic array.
    * Specifically, the expression {fromPoly a} is equivalent to the
    * expression
    *
    *> tabulate (Array.length a, fn i => Array.sub (a, i))
    *)

   val fromVector : vector -> array
   (**
    * Creates a new array from the given vector.  Specifically, the
    * expression {fromVector v} is equivalent to the expression
    *
    *> tabulate (Vector.length v, fn i => Vector.sub (v, i))
    *)

   val toList : array -> elem list
   (**
    * Generates a list from the given array.  Specifically, the result of
    * {toList a} is equivalent to {foldr op :: [] a}.
    *)

   val toPoly : array -> elem Array.array
   (**
    * Creates a new polymorphic array from the given monomorphic array.
    * Specifically, the expression {toPoly a} is equivalent to the
    * expression
    *
    *> Array.tabulate (length a, fn i => Array.sub (a, i))
    *)

   val toVector : array -> vector
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

   val isoList : (array, elem list) Iso.iso
   (**
    * An isomorphism between arrays and lists.  It is always equivalent to
    * {(toList, fromList)}.  Note that the isomorphism does not preserve
    * identity.
    *)

   val isoPoly : (array, elem Array.array) Iso.iso
   (**
    * An isomorphism between monomorphic and polymorphic arrays.  It is
    * always equivalent to {(toPoly, fromPoly)}.  Note that the
    * isomorphism does not preserve identity.
    *)

   val isoVector : (array, vector) Iso.iso
   (**
    * An isomorphism between arrays and vectors.  It is always equivalent
    * to {(toVector, fromVector)}.  Note that the isomorphism does not
    * preserve identity.
    *)
end
