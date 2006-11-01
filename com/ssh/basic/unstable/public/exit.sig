(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Signature for exit (or escape) handlers.
 *
 * Note the type of the {to} function.  The return type of {to} is a type
 * variable that only appears as the return type.  This means that the
 * {to} function doesn't return normally to the caller and that you can
 * call it from a context of any type.
 *)
signature EXIT = sig
   type 'a t
   (** The type of exits. *)

   val within : ('a t -> 'a) -> 'a
   (**
    * Sets up an exit and passes it to the given function.  The function
    * may then either return normally or by calling {to} with the exit and
    * a return value.  For example,
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
    * exit {l} with the value {v}.
    *
    * Note that {to} works by raising an exception.  The exception can be
    * caught by a wildcard exception handler.  Wildcard exception handlers
    * should usually reraise the exception after performing their effects.
    * Also, if the {within} invocation that introduced the exit {l} has
    * already returned, the effect is (still) that an exception will be
    * raised.
    *)
end
