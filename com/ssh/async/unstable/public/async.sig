(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for an asynchronous programming library.
 *
 * The design is based on a library posted by Stephen Weeks to the
 * MLton-user mailing list:
 * [http://mlton.org/pipermail/mlton-user/2006-July/000856.html
 *  Simple, portable, asynchronous programming in SML].
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
       *
       * {runAll} is typically only called from a platform dependent event
       * loop.
       *)
   end

   structure Event : sig
      type 'a t
      (**
       * An event of type {'a t} is a first-class object that represents
       * the possibility of communicating a value of type {'a} from one
       * part of the program to another.  At each moment, an event is
       * either enabled or disabled depending on whether it is able to
       * supply a value.  When an event is enabled, it may be committed
       * to, which means that the value supplied by the event is consumed.
       *)
   end

   (** == Combinators ==
    *
    * Event combinators work in such away that committing to the returned
    * event also commits to a given event.  However, committing to a given
    * event does not commit to the returned event.
    *)

   val on : 'a Event.t -> ('a -> 'b) -> 'b Event.t
   (**
    * Creates an event that is enabled whenever the given event is enabled
    * and when committed to also executes the given function, which is
    * usually referred to as either a handler or an action.
    *)

   val choose : 'a Event.t List.t -> 'a Event.t
   (**
    * Creates an event that chooses, in an unspecified manner, an enabled
    * event from the given list of events to commit to.
    *)

   (** == Handling Events == *)

   val once : Unit.t Event.t Effect.t
   (**
    * Registers desire to commit to the given event when it is enabled.
    *
    * {once} returns without running handlers.  Any handlers attached to a
    * committed event are executed only when {Handler.runAll} is called.
    *
    * It is possible to register desire to commit to a particular event
    * multiple times and an event may not be able to supply a value for
    * all of the commits.
    *)

   (** == Utilities == *)

   val each : Unit.t Event.t Effect.t
   (**
    * Registers desire to commit to the given event each time it occurs.
    *
    * {each} can be implemented as a simple tail-recursive loop:
    *
    *> fun each e = when e (fn () => each e)
    *)

   val when : 'a Event.t -> 'a Effect.t Effect.t
   (** {when e = once o on e} *)

   val every : 'a Event.t -> 'a Effect.t Effect.t
   (** {every e = each o on e} *)

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

   structure SkipCh : sig
      type 'a t
      val new : 'a t Thunk.t
      val take : 'a t -> 'a Event.t
      val send : 'a t -> 'a Effect.t
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
