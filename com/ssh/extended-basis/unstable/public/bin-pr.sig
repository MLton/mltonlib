(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with binary predicates. *)
signature BIN_PR = sig
   type 'a t = 'a Sq.t UnPr.t
   (** Type of binary predicates or relations (e.g. {<, <=, >=, >, ...}). *)
end
