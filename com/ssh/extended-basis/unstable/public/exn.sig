(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with exceptions. *)
signature EXN = sig
   type t = exn

   val addMessager : (t -> String.t Option.t) Effect.t
   (**
    * Adds a pretty-printer to be used by {message} for converting
    * exceptions to strings.  Messagers are tried in order from most
    * recently added to least recently added.
    *)

   (** == Examining Exceptions == *)

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
