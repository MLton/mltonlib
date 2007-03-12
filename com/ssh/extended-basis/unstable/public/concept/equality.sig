(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Equality ==
 *
 * An equality type (not necessarily an {eqtype}) has some natural
 * equality relation.
 *)

signature EQUALITY_CORE = sig
   type equality

   val == : equality BinPr.t
   (** {i == j} returns {true} iff {i} is equal to {j}. *)
end

signature EQUALITY_EX = sig
   type equality_ex

   val != : equality_ex BinPr.t
   (** {i != j} returns {true} iff {i} is not equal to {j}. *)
end

signature EQUALITY = sig
   include EQUALITY_CORE EQUALITY_EX
   sharing type equality = equality_ex
end
