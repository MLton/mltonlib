(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with thunks. *)
signature THUNK = sig
   type 'a t = unit -> 'a
   (** The type of thunks or suspended computations (e.g. {fn () => exp}). *)

   val mk : 'a -> 'a t
   (** Constant thunk ({thunk x = let val x = x in fn () => x end}). *)
end
