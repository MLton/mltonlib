(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Scoped resource management combinators. *)
signature WITH = sig
   type 'a t

   (** == Monad Interface == *)

   include MONAD_CORE where type 'a monad = 'a t

   structure Monad : MONAD where type 'a monad = 'a t

   (** === Lifting Ad Hoc SRM Combinators === *)

   val lift : 'a Effect.t Effect.t -> 'a t
   (** Lifts an arbitrary SRM combinator to the monad. *)

   (** === Running With === *)

   val for : 'a t -> 'a Effect.t Effect.t
   (**
    * Runs the monad and passes the value to the effect block.  This may
    * be more efficient than {one}.
    *)

   val one : 'a t -> ('a, 'b) CPS.t
   (**
    * Runs the monad and passes the value to the given block.  The result
    * of the block is then returned.  If the result is {()} then it is
    * better to use {for}.
    *)

   (** == Primitives == *)

   val alloc : ('a -> 'b) -> 'a -> 'b t
   (**
    * Apply the given function with the given value just before entry to
    * the block.
    *
    * This is basically a lazy version of {return}.  Specifically, {alloc
    * g a} is equivalent to {fn f => f (g a)}, assuming {g} and {a} are
    * variables.
    *)

   val free : 'a Effect.t -> 'a -> 'a t
   (**
    * Performs the effect with the given value after exit from the block.
    * This is basically a variation of {finally}.  Specifically, {free ef
    * x f} is equivalent to {finally (fn () => f x, fn () => ef x)}.
    *)

   (** == Useful Combinations == *)

   val around : 'a Thunk.t -> 'a Effect.t -> 'a t
   (**
    * Allocate resources with given thunk before entry to the block and
    * release the resource with given effect after exit from the block.
    * {around new del} is equivalent to {alloc new () >>= free del}.
    *)

   val entry : Unit.t Effect.t -> Unit.t t
   (**
    * Perform given effect before entry to the block.
    *
    * Note that the identifier {before} is already used in the Standard ML
    * Basis Library.
    *)

   val exit : Unit.t Effect.t -> Unit.t t
   (** Perform given effect after exit from the block. *)

   val calling : {entry : 'a Effect.t, exit : 'a Effect.t} -> 'a -> Unit.t t
   (**
    * Call given effects with the given value before entry to and after
    * exit from the block.
    *)

   val passing : 'a Effect.t -> {entry : 'a, exit : 'a} -> Unit.t t
   (**
    * Call given effect with a given values before entry to and after exit
    * from the block.
    *)
end
