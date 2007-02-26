(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature ASYNC = sig
   exception Put

   (** == Handlers == *)

   structure Handler : sig
      val runAll : Unit.t Effect.t
   end

   (** == Events == *)

   structure Event : sig
      type 'a t

      (** == Combinators == *)

      val on : 'a t * ('a -> 'b) -> 'b t
      val choose : 'a t List.t -> 'a t

      (** == Handling Events == *)

      val once : Unit.t t Effect.t
      val each : Unit.t t Effect.t

      (** == Utilities == *)

      val when : ('a t * 'a Effect.t) Effect.t
      (** {when (e, h) = once (on (e, h))} *)

      val every : ('a t * 'a Effect.t) Effect.t
      (** {every (e, h) = each (on (e, h))} *)

      val any : Unit.t t List.t Effect.t
      (** {any = once o choose} *)
   end

   (** == Communication Mechanisms == *)

   structure Ch : sig
      type 'a t
      val new : 'a t Thunk.t
      val take : 'a t -> 'a Event.t
      val give : 'a t -> 'a -> Unit.t Event.t
   end

   structure IVar : sig
      type 'a t
      val new : 'a t Thunk.t
      val read : 'a t -> 'a Event.t
      val put : 'a t -> 'a Effect.t
   end

   structure MVar : sig
      type 'a t
      val new : 'a t Thunk.t
      val take : 'a t -> 'a Event.t
      val put : 'a t -> 'a Effect.t
   end

   structure Mailbox : sig
      type 'a t
      val new : 'a t Thunk.t
      val take : 'a t -> 'a Event.t
      val send : 'a t -> 'a Effect.t
   end

   structure Multicast : sig
      type 'a t
      val new : 'a t Thunk.t
      val taker : 'a t -> 'a Event.t
      val send : 'a t -> 'a Effect.t
   end
end
