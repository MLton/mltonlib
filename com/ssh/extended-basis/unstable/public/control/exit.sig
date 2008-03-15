(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for exit (or escape) handlers.
 *
 * Note that the implementation necessarily uses exception handling.  This
 * is to make proper resource handling possible.  Exceptions raised by the
 * implementation can be caught by wildcard exception handlers.  Wildcard
 * exception handlers should generally reraise exceptions after performing
 * their effects.
 *)
signature EXIT = sig
   type 'a t
   (** The type of exits. *)

   val within : ('a t, 'a) CPS.t
   (**
    * Sets up an exit and passes it to the given function.  The function
    * may then return normally or by calling {to} with the exit and a
    * return value.  For example,
    *
    *> Exit.within
    *>    (fn l =>
    *>        if condition then
    *>           Exit.to l 1
    *>        else
    *>           2)
    *
    * evaluates either to {1} or to {2} depending on the {condition}.
    *
    * Note that the function receiving the exit is called from a non-tail
    * position.
    *)

   val to : 'a t -> 'a -> 'b
   (**
    * {to l v} returns from the {within} invocation that introduced the
    * exit {l} with the value {v}.  Evaluating {to l v} outside of the
    * {within} invocation that introduced {l} is a programming error and
    * raises an exception.
    *
    * Note that the type variable {'b} only appears as the return type.
    * This means that {to} doesn't return normally to the caller and can
    * be called from a context of any type.
    *)

   val call : ('a -> 'b, 'a) CPS.t
   (**
    * Simpler, but less flexibly typed, interface to {within} and {to}.
    * Specifically, {call f} is equivalent to {within (f o to)}.
    *)
end
