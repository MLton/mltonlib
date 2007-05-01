(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A bitwise operable type can be treated as a set of bits.  The set of
 * bits may be finite or infinite.
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
end
