(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with (unary) predicates. *)
signature UN_PR = sig
   type 'a t = 'a -> bool
   (** Type of (unary) predicates (e.g. {null, isSome, ...}). *)

   val andAlso : 'a t BinOp.t
   (** Conjunction of predicates ({(p andAlso q) x = p x andalso q y}). *)

   val negate : 'a t UnOp.t
   (** Predicate negation ({negate p = not o p}). *)

   val orElse : 'a t BinOp.t
   (** Disjunction of predicates ({(p orElse q) x = p x orelse q x}). *)
end
