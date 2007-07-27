(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with thunks. *)
signature THUNK = sig
   type 'a t = Unit.t -> 'a
   (** The type of thunks or suspended computations (e.g. {fn () => exp}). *)

   val mk : 'a -> 'a t
   (** Constant thunk ({thunk x = let val x = x in fn () => x end}). *)

   val iso : ('a, 'a t) Iso.t
   (** The trivial isomorphism between values and thunks. *)
end
