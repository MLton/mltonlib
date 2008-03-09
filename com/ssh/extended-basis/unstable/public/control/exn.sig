(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with exceptions. *)
signature EXN = sig
   type t = exn
   (** Convenience alias. *)

   (** == Exception Handling == *)

   val apply : ('a -> 'b) -> 'a -> (t, 'b) Sum.t
   (** Perform an application ({apply f x = INR (f x) handle e => INL e}). *)

   val eval : 'a Thunk.t -> (t, 'a) Sum.t
   (** Evaluate a thunk ({eval th = INR (th ()) handle e => INL e}). *)

   val reflect : (t, 'a) Sum.t -> 'a
   (** {reflect} is equivalent to {sum (throw, id)}. *)

   val after : 'a Thunk.t * Unit.t Effect.t -> 'a
   (** {after (th, ef) = try (th, past ef, throw o past ef)}. *)

   val finally : 'a Thunk.t * Unit.t Effect.t -> 'a
   (** DEPRECATED: This is an Alice ML reserved word. *)

   val throw : t -> 'a
   (** Raise exception ({throw exn = raise exn}). *)

   val try : 'a Thunk.t * ('a -> 'b) * (t -> 'b) -> 'b
   (**
    * Try-in-unless ({try (th, fv, fe) = sum (fv, fe) (eval th)}).  {try}
    * facilitates fine control over exception handling.  {try} implements
    * the try-in-unless construct of Benton and Kennedy.
    *)

   (** == Examining Exceptions == *)

   val addMessager : (t -> String.t Option.t) Effect.t
   (**
    * Adds a pretty-printer to be used by {message} for converting
    * exceptions to strings.  Messagers are tried in order from most
    * recently added to least recently added.
    *)

   val message : t -> String.t
   (** Same as {General.exnMessage}. *)

   val name : t -> String.t
   (** Same as {General.exnName}. *)

   val history : t -> String.t List.t
   (**
    * Returns call stack at the point that the exception was first raised.
    * Each element of the list is a file position.  The elements are in
    * reverse chronological order, i.e. the function called last is at the
    * front of the list.
    *
    * {history} will likely return {[]} unless the program is compiled
    * with a compiler dependent option to support exception history.
    *)
end
