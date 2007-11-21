(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature CERMLANG = sig
   structure Proc : sig
      type t
   end

   structure Msg : sig
      type t = Exn.t
   end

   exception Time

   val start : Unit.t Effect.t Effect.t

   val spawn : Unit.t Effect.t -> Proc.t
   val self : Proc.t Thunk.t
   val recvIn : Time.time Option.t -> (Msg.t -> 'a Thunk.t) -> 'a
   val recv : (Msg.t -> 'a Thunk.t) -> 'a
   val <- : (Proc.t * Msg.t) Effect.t
end
