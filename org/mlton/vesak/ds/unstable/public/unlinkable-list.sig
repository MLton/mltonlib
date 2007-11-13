(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature UNLINKABLE_LIST = sig
   type 'a t

   val new : 'a t Thunk.t

   val pushFront : 'a t -> 'a -> Unit.t Effect.t
   val pushBack : 'a t -> 'a -> Unit.t Effect.t

   val popFront : 'a t -> 'a Option.t
   val popBack : 'a t -> 'a Option.t
end
