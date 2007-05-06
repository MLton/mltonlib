(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A shiftable type can be shifted like a sequence of bits.  The sequence
 * of bits may be finite or infinite.  In an infinite sequence, the most
 * significant (or leftmost) bits are either all zeros or all ones.  Zero
 * extending shift operator, {>>}, does not work with such infinite bit
 * sequences and is not specified.
 *)
signature SHIFTABLE = sig
   type shiftable

   val << : shiftable ShiftOp.t
   (**
    * {i << n} shifts {i} to the left by {n} bit positions, filling in
    * zeros from the right.  When {i} and {n} are interpreted as integers,
    * the latter non-negative, this returns {i * 2^n}.
    *)

   val ~>> : shiftable ShiftOp.t
   (**
    * {i ~>> n} shifts {i} to the right by {n} bit positions.  When {i}
    * and {n} are interpreted as integers, the latter non-negative, this
    * returns {floor (i / 2^n)}.
    *)
end

(** Like {SHIFTABLE}, but the sequence of bits is finite. *)
signature SHIFTABLE_FIN = sig
   include SHIFTABLE
   val >> : shiftable ShiftOp.t
end
