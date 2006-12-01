(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with unary operators. *)
signature UN_OP = sig
   type 'a t = 'a -> 'a
   (** Type of unary operators (e.g. {~, rev, ...}). *)
end
