(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Asynchronous programming interface.
 *)
signature ASYNC = sig
   exception Full
   (**
    * Raised by {IVar.fill} and {MVar.fill} when an attempt is made to
    * fill a variable that already holds a value.
    *)

   structure Handler : sig
      val runAll : Unit.t Effect.t
      (**
       * Attempts to run all scheduled handlers.  This includes handlers
       * that are scheduled while {runAll} is running.  {runAll} stops
       * either when all handlers have been executed successfully or when
       * a handler raises an exception in which case the exception is
       * propagated to the caller of {runAll}.
       *)
   end

   structure Event : sig
      type 'a t
      (** The type of asynchronous events. *)
   end

   (** == Combinators == *)

   val on : 'a Event.t * ('a -> 'b) -> 'b Event.t
   (**
    * Creates an event that acts like the given event and also executes
    * the given function on the event value when the created event is
    * committed.
    *)

   val choose : 'a Event.t List.t -> 'a Event.t
   (**
    * Creates an event that chooses, in an unspecified manner, an occured
    * event from the given list of events to commit.
    *)

   (** == Handling Events == *)

   val once : Unit.t Event.t Effect.t
   (**
    * Commit to the given event once when it occurs.  The handlers
    * attached to a committed event are executed when {Handler.runAll} is
    * called.
    *)

   (** == Utilities == *)

   val each : Unit.t Event.t Effect.t
   (**
    * Commit to the given event each time it occurs.  {each} can be
    * implemented as
    *
    *> fun each e = when (e, fn () => each e)
    *)

   val when : ('a Event.t * 'a Effect.t) Effect.t
   (** {when (e, h) = once (on (e, h))} *)

   val every : ('a Event.t * 'a Effect.t) Effect.t
   (** {every (e, h) = each (on (e, h))} *)

   val any : Unit.t Event.t List.t Effect.t
   (** {any = once o choose} *)

   val all : Unit.t Event.t List.t Effect.t
   (** {all = each o choose} *)

   (** == Communication Mechanisms ==
    *
    * The names of operations have been chosen to communicate the semantics:
    * - A rendezvous is needed to {give} a value to a handler.
    * - One can't {fill} a variable twice without emptying it in between.
    * - Many can {read} a value without taking it.
    * - One can {send} a value to a handler without a rendezvous.
    * - Only the handler that {take}s a value sees it.
    * - Multiple {taker}s may observe the same sequence of values.
    *)

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
      val fill : 'a t -> 'a Effect.t
   end

   structure MVar : sig
      type 'a t
      val new : 'a t Thunk.t
      val take : 'a t -> 'a Event.t
      val fill : 'a t -> 'a Effect.t
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
