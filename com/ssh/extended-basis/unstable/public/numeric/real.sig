(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {REAL} signature. *)
signature REAL = sig
   include BASIS_REAL

   type t = real
   (** Convenience alias. *)

   (** == Numeric == *)

   val sq : t UnOp.t (** {sq x = x * x} *)

   (** == Embeddings == *)

   val embDecimal : (t, IEEEReal.decimal_approx) Emb.t
   (**
    * An embedding of reals into decimal approximations.  It is always
    * equivalent to {(toDecimal, fromDecimal)}.
    *)

   val embString : (t, String.t) Emb.t
   (**
    * An embedding of reals into strings.  It is always equivalent to
    * {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : IEEEReal.rounding_mode -> (t, Int.t) Iso.t
   (**
    * Returns an isomorphism between integers of type {Int.t} and reals
    * given a rounding mode.  Specifically, the result of {isoInt mode} is
    * equivalent to {(toInt mode, fromInt)}.  Note that the result isn't
    * an isomorphism in the mathematical sense.
    *)

   val isoLarge : IEEEReal.rounding_mode -> (t, LargeReal.t) Iso.t
   (**
    * Returns an isomorphism between reals of type {LargeReal.t} and
    * reals of type {real} given a rounding mode.  Specifically, the
    * result of {isoLarge mode} is equivalent to {(toLarge, fromLarge
    * mode)}.  Note that the result isn't an isomorphism in the
    * mathematical sense.
    *)

   val isoLargeInt : IEEEReal.rounding_mode -> (t, LargeInt.t) Iso.t
   (**
    * Returns an isomorphism between integers of type {LargeInt.t} and
    * reals given a rounding mode.  Specifically, the result of
    * {isoLargeInt mode} is equivalent to {(toLargeInt mode,
    * fromLargeInt)}.  Note that the result isn't an isomorphism in the
    * mathematical sense.
    *)

   val isoManExp : (t, {man : t, exp : Int.t}) Iso.t
   (**
    * An isomorphism between reals and their representation as a mantissa
    * and an exponent.  It is always equivalent to {(toManExp,
    * fromManExp)}.
    *)
end
