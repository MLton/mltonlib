(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {REAL} signature.
 *)
signature REAL = sig
   include REAL

   (** == Embeddings == *)

   val embDecimal : (real, IEEEReal.decimal_approx) emb
   (**
    * An embedding of reals into decimal approximations.  It is always
    * equivalent to {(toDecimal, fromDecimal)}.
    *)

   val embString : (real, string) emb
   (**
    * An embedding of reals into strings.  It is always equivalent to
    * {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : IEEEReal.rounding_mode -> (real, Int.int) iso
   (**
    * Returns an isomorphism between integers of type {Int.int} and reals
    * given a rounding mode.  Specifically, the result of {isoInt mode} is
    * equivalent to {(toInt mode, fromInt)}.  Note that the result isn't
    * an isomorphism in the mathematical sense.
    *)

   val isoLarge : IEEEReal.rounding_mode -> (real, LargeReal.real) iso
   (**
    * Returns an isomorphism between reals of type {LargeReal.real} and
    * reals of type {real} given a rounding mode.  Specifically, the
    * result of {isoLarge mode} is equivalent to {(toLarge, fromLarge
    * mode)}.  Note that the result isn't an isomorphism in the
    * mathematical sense.
    *)

   val isoLargeInt : IEEEReal.rounding_mode -> (real, LargeInt.int) iso
   (**
    * Returns an isomorphism between integers of type {LargeInt.int} and
    * reals given a rounding mode.  Specifically, the result of
    * {isoLargeInt mode} is equivalent to {(toLargeInt mode,
    * fromLargeInt)}.  Note that the result isn't an isomorphism in the
    * mathematical sense.
    *)

   val isoManExp : (real, {man : real, exp : int}) iso
   (**
    * An isomorphism between reals and their representation as a mantissa
    * and an exponent.  It is always equivalent to {(toManExp,
    * fromManExp)}.
    *)
end
