(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signed type can be thought of as having a separate sign-bit, which is
 * negative or positive, and an absolute value, which is considered
 * positive.
 *)
signature SIGNED = sig
   type signed

   val abs : signed UnOp.t
   (** Returns the absolute value of the argument. *)

   val copySign : signed BinOp.t
   (**
    * {copySign (x, y)} returns {x} with the sign of {y}.  Raises
    * {Overflow} if the result is not representable.
    *)

   val sameSign : signed BinPr.t
   (** {sameSign (x, y)} is equivalent to {signBit x = signBit y}. *)

   val sign : signed -> Int.t
   (**
    * Returns {~1} if the argument is negative, {0} if it is zero, or {1}
    * if it is positive.  Raises {Domain} if the result can not be
    * determined (e.g. the argument does not have a value in the usual
    * sense).
    *)

   val signBit : signed UnPr.t
   (** Returns true if and only if the sign of the argument is negative. *)
end
