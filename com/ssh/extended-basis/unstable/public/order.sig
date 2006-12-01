(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with the {order} datatype. *)
signature ORDER = sig
   datatype t = datatype order
   (** The {order} datatype. *)

   val swap : t UnOp.t
   (**
    * Swap order:
    *
    *> swap EQUAL   = EQUAL
    *> swap GREATER = LESS
    *> swap LESS    = GREATER
    *)

   val orWhenEq : t * t Thunk.t -> t
   (**
    * Sequencing of comparisons.  {orWhenEq (a, bTh)} is equivalent to
    *
    *> case a of
    *>    EQUAL => bTh ()
    *>  | other => other
    *)

   (** == Predicates == *)

   val isEqual : t UnPr.t    (** {isEqual x = x = EQUAL} *)
   val isGreater : t UnPr.t  (** {isGreater x = x = GREATER} *)
   val isLess : t UnPr.t     (** {isLess x = x = LESS} *)
end
