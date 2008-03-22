(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with (unary) predicates. *)
signature UN_PR = sig
   type 'a t = 'a -> Bool.t
   (** Type of (unary) predicates (e.g. {null, isSome, ...}). *)

   val map : ('a -> 'b) -> 'b t -> 'a t
   (** Change the domain of a predicate. *)

   val andAlso : 'a t BinOp.t
   (** Conjunction of predicates ({(p andAlso q) x = p x andalso q y}). *)

   val neg : 'a t UnOp.t
   (** Predicate negation ({neg p = not o p}). *)

   val orElse : 'a t BinOp.t
   (** Disjunction of predicates ({(p orElse q) x = p x orelse q x}). *)
end
