(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {INTEGER} signature. *)
signature INTEGER = sig
   include BASIS_INTEGER

   type t = int
   (** Convenience alias. *)

   (** == Bounds == *)

   val bounds : t Sq.t Option.t
   (**
    * Pair of the minimal and maximal integers, respectively,
    * representable by {int}.  If {minInt = NONE} and {maxInt = NONE},
    * this is also {NONE}.  Otherwise this is {SOME (valOf minInt, valOf
    * maxInt)}.
    *)

   (** == Embeddings == *)

   val embString : (t, String.t) Emb.t
   (**
    * An embedding of integers into strings.  It is always equivalent to
    * {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : (t, Int.t) Iso.t
   (**
    * An isomorphism between integers of type {int} and the default
    * integer type.  It is always equivalent to {(toInt, fromInt)}.  Note
    * that one of the injection and projection parts may be partial.
    *)

   val isoLarge : (t, LargeInt.t) Iso.t
   (**
    * An isomorphism between integers of type {int} and integers of type
    * {LargeInt.int}.  It is always equivalent to {(toLarge, fromLarge)}.
    * Note that the projection part may be partial.
    *)

   (** == Predicates == *)

   val isEven : t UnPr.t
   (**
    * Returns true if the given integer is of the form {2*n} for some
    * integer {n}.
    *)

   val isOdd : t UnPr.t
   (**
    * Returns true if the given integer is of the form {2*n+1} for some
    * integer {n}.
    *)

   val isZero : t UnPr.t
   (** Returns true if the given integer is {0}. *)
end
