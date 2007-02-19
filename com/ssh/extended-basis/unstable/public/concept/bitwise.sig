(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A bitwise operable type can be treated as a sequence of bits.  The
 * sequence of bits may be finite or infinite.  In an infinite sequence,
 * the most significant (or leftmost) bits are either all zeros or all
 * ones.  Zero extending shift operator, {>>}, does not work with such
 * infinite bit sequences and is not specified.
 *)
signature BITWISE = sig
   type bitwise

   val andb : bitwise BinOp.t
   (** Returns the bitwise AND of the arguments. *)

   val notb : bitwise UnOp.t
   (**
    * Returns the bitwise complement NOT of the argument.  When values are
    * interpreted as two's complement integers {notb i = ~(i + 1)}.
    *)

   val orb : bitwise BinOp.t
   (** Returns the bitwise OR of the arguments. *)

   val xorb : bitwise BinOp.t
   (** Returns the bitwise exclusive OR of the arguments. *)

   val << : bitwise ShiftOp.t
   (**
    * {i << n} shifts {i} to the left by {n} bit positions, filling in
    * zeros from the right.  When {i} and {n} are interpreted as integers,
    * the latter non-negative, this returns {i * 2^n}.
    *)

   val ~>> : bitwise ShiftOp.t
   (**
    * {i ~>> n} shifts {i} to the right by {n} bit positions.  When {i}
    * and {n} are interpreted as integers, the latter non-negative, this
    * returns {floor (i / 2^n)}.
    *)
end
