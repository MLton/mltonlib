(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with binary operators. *)
signature BIN_OP = sig
   type 'a t = 'a Sq.t -> 'a
   (** Type of binary operators (e.g. {+, -, @, ...}). *)

   val map : ('b, 'a) Iso.t -> 'a t -> 'b t
   (** Change the sort of a binary operator. *)
end
