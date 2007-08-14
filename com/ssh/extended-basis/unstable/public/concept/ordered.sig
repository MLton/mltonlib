(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Ordered ==
 *
 * An ordered type implements a total order.  Note that it is possible to
 * implement this signature, but not strictly fulfill the requirements of
 * a total order.  This would be the case with {Real}.
 *)

signature ORDERED_CORE = sig
   type ordered

   val compare : ordered Cmp.t
   (**
    * {compare (i, j)} returns {LESS}, {EQUAL}, or {GREATER} when {i} is
    * less than, equal to, or greater than {j}, respectively.
    *)
end

signature ORDERED_EX = sig
   type ordered_ex

   val < : ordered_ex BinPr.t
   (** {i < j} returns {true} iff {i} is less than {j}. *)

   val <= : ordered_ex BinPr.t
   (** {i <= j} returns {true} iff {i} is less than or equal to {j}. *)

   val > : ordered_ex BinPr.t
   (** {i > j} returns {true} iff {i} is greater than {j}. *)

   val >= : ordered_ex BinPr.t
   (** {i >= j} returns {true} iff {i} is greater than or equal to {j}. *)

   val max : ordered_ex BinOp.t
   (** Returns the larger of the arguments. *)

   val min : ordered_ex BinOp.t
   (** Returns the smaller of the arguments. *)

   val inRange : ordered_ex Sq.t -> ordered_ex UnPr.t
   (**
    * {inRange (lo, hi) v} is equivalent to {lo <= v andalso v <= hi},
    * except that {inRange (lo, hi)} raises {Domain} iff {hi < lo}.
    *)
end

signature ORDERED = sig
   include ORDERED_CORE EQUALITY ORDERED_EX
   sharing type ordered = equality = ordered_ex
end
