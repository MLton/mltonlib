(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with binary predicates. *)
signature BIN_PR = sig
   type 'a t = 'a Sq.t UnPr.t
   (** Type of binary predicates or relations (e.g. {<, <=, >=, >, ...}). *)

   val map : ('a -> 'b) -> 'b t -> 'a t
   (** Change the domain of a binary predicate. *)
end
