(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Signature for an imperative polymorphic queue.
 *)

signature QUEUE = sig
   type 'a t

   val new : 'a t Thunk.t

   val isEmpty : 'a t UnPr.t

   val deque : 'a t -> 'a Option.t
   val enque : 'a t -> 'a Effect.t

   val foldClear : ('a * 's -> 's) -> 's -> 'a t -> 's
   val appClear : 'a Effect.t -> 'a t Effect.t
end
