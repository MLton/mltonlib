(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Scoped resource management combinators. *)
signature WITH = sig
   type ('a, 'b) t = ('a -> 'b) -> 'b
   (**
    * Type for a form of continuation-passing style.
    *
    * In this context, a function of type {('a -> 'b) -> 'b} is referred
    * to as a "with -procedure", and a continuation, of type {'a -> 'b},
    * given to a with -procedure is called a "block".
    *)

   (** == Monad Interface == *)

   val return : 'a -> ('a, 'r) t
   (** Calls the block with the specified value.  Also see {alloc}. *)

   val >>= : ('a, 'r) t * ('a -> ('b, 'r) t) -> ('b, 'r) t
   (**
    * Composes two with -procedures, passing any value produced by the
    * first as an argument to the second.
    *)

   (** == Primitives == *)

   val alloc : ('a -> 'b) -> 'a -> ('b, 'r) t
   (**
    * Apply the given function with the given value just before entry to
    * the block.
    *
    * This is basically a lazy version of {return}.  Specifically, {alloc
    * g a} is equivalent to {fn f => f (g a)}, assuming {g} and {a} are
    * variables.
    *)

   val free : 'a Effect.t -> 'a -> ('a, 'r) t
   (**
    * Performs the effect with the given value after exit from the block.
    * This is basically a variation of {finally}.  Specifically, {free ef
    * x f} is equivalent to {finally (fn () => f x, fn () => ef x)}.
    *)

   (** == Useful Combinations == *)

   val >>& : ('a, 'r) t * ('b, 'r) t -> (('a, 'b) Product.t, 'r) t
   (** Product combinator. *)

   val around : 'a Thunk.t -> 'a Effect.t -> ('a, 'r) t
   (**
    * Allocate resources with given thunk before entry to the block and
    * release the resource with given effect after exit from the block.
    * {around new del} is equivalent to {alloc new () >>= free del}.
    *)

   val entry : Unit.t Effect.t -> (Unit.t, 'r) t
   (**
    * Perform given effect before entry to the block.
    *
    * Note that the identifier {before} is already used in the Standard ML
    * Basis Library.
    *)

   val exit : Unit.t Effect.t -> (Unit.t, 'r) t
   (** Perform given effect after exit from the block. *)

   val calling :
       {entry : 'a Effect.t, exit : 'a Effect.t} -> 'a -> (Unit.t, 'r) t
   (**
    * Call given effects with the given value before entry to and after
    * exit from the block.
    *)

   val passing : 'a Effect.t -> {entry : 'a, exit : 'a} -> (Unit.t, 'r) t
   (**
    * Call given effect with a given values before entry to and after exit
    * from the block.
    *)
end
