(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with binary functions. *)
signature BIN_FN = sig
   type ('a, 'b) t = 'a Sq.t -> 'b
   (** Type of binary functions. *)

   val map : ('c -> 'a) * ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
   (** Change the domain and range of a binary function. *)
end
