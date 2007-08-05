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

   val map : ('a -> 'b) -> 'a t -> 'b t
   (** Change the type of a thunk. *)

   val isoValue : ('a t, 'a) Iso.t
   (** The trivial isomorphism between values and thunks. *)

   val iso : ('a, 'b) Iso.t -> ('a t, 'b t) Iso.t
   (** Lifts an iso between values to an iso between thunks. *)
end
