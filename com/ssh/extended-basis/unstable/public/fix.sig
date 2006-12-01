(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with fixpoints. *)
signature FIX = sig
   type 'a t = 'a UnOp.t -> 'a
   (** The type of fixpoint combinators. *)

   exception Fix
   (** Exception for reporting errors in computing fixpoints. *)
end
