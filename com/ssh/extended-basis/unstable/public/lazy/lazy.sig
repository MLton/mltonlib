(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Lazy promises.
 *
 * The design is based on [http://srfi.schemers.org/srfi-45/ SRFI-45]
 * ``Primitives for Expressing Iterative Lazy Algorithms'' by André van
 * Tonder.
 *
 * The general recipe to express lazy algorithms is to
 * - wrap all constructors with {delay (fn () => ...)},
 * - apply {force} to arguments of destructors, and
 * - wrap function bodies with {lazy (fn () => ...)}.
 *)
signature LAZY = sig
   type 'a t
   (** The abstract type of promises. *)

   val delay : 'a Thunk.t -> 'a t
   (**
    * Takes a thunk of type {'a thunk} and returns a promise of type
    * {'a t} which at some point in the future may be asked (by the
    * {force} procedure) to evaluate the thunk and deliver the
    * resulting value.
    *)

   val eager : 'a -> 'a t
   (**
    * Takes an argument of type {'a} and returns a promise of type
    * {'a t}.  As opposed to {delay}, the argument is evaluated eagerly.
    *
    * Semantically, writing
    *
    *> eager expression
    *
    * is equivalent to writing
    *
    *> let val value = expression in delay (fn () => value) end
    *
    * However, the former is more efficient since it does not require
    * unnecessary creation and evaluation of thunks.  We also have the
    * equivalence
    *
    *> delay (fn () => expression) = lazy (eager expression)
    *
    * assuming that evaluation of the expression does not raise an
    * exception.
    *)

   val force : 'a t -> 'a
   (**
    * Takes a promise of type {'a t} and returns a value of type {'a}
    * as follows: If a value of type {'a} has been computed for the
    * promise, this value is returned.  Otherwise, the promise is first
    * evaluated, then overwritten by the obtained promise or value, and
    * then force is again applied (iteratively) to the promise.
    *)

   val lazy : 'a t Thunk.t -> 'a t
   (**
    * Takes a thunk returning a promise of type {'a t} and returns a
    * promise of type {'a t} which at some point in the future may be
    * asked (by the {force} procedure) to evaluate the thunk and
    * deliver the resulting promise.
    *)

   val memo : 'a Thunk.t UnOp.t
   (** {memo th} is equivalent to {toThunk (delay th)}. *)

   val toThunk : 'a t -> 'a Thunk.t
   (**
    * Converts a promise into a thunk.  This can be useful for working
    * around the value restriction, for example.
    *)

   val Y : 'a t Tie.t
   (** Fixpoint tier for promises. *)
end
