(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with fixpoints. *)
signature FIX = sig
   type 'a t = 'a UnOp.t -> 'a
   (** The type of fixpoint combinators. *)

   exception Fix
   (** Exception for reporting errors in computing fixpoints. *)
end
