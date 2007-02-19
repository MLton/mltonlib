(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with binary operators. *)
signature BIN_OP = sig
   type 'a t = ('a, 'a) BinFn.t
   (** Type of binary operators (e.g. {+, -, @, ...}). *)

   val map : ('b, 'a) Iso.t -> 'a t -> 'b t
   (** Change the sort of a binary operator. *)
end
