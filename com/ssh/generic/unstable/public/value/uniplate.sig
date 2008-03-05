(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic function for processing recursive datatypes.
 * Unlike the {Reduce}, {Transform}, and {Fmap} generics, this generic
 * allows recursive datatypes to be processed in various ways without
 * requiring the recursive datatype to be encoded as a fixed point of a
 * functor.
 *
 * Much of this generic is inspired by the following article:
 *
 *   Uniform Boilerplate and List Processing
 *   Neil Mitchell and Colin Runciman
 *   Haskell'07, September 30, 2007, Freiburg, Germany
 *   [http://www-users.cs.york.ac.uk/~ndm/uniplate/]
 *)
signature UNIPLATE = sig
   structure UniplateRep : OPEN_REP

   val children : ('a, 'x) UniplateRep.t -> 'a -> 'a List.t
   (**
    * Returns all maximal proper substructures of the same type contained
    * in the given value.  This is non-recursive.
    *)

   val universe : ('a, 'x) UniplateRep.t -> 'a -> 'a List.t
   (**
    * Returns a list of all substructures of the same type contained in
    * the given value (including it).  This is recursive.
    *)

   val holes : ('a, 'x) UniplateRep.t -> 'a -> ('a * 'a UnOp.t) List.t
   (**
    * Returns a list of all maximal proper substructures of the given
    * value and functions to replace the corresponding substructure in the
    * given value.
    *
    *> map op </ (holes t x) = children t x
    *)

   val contexts : ('a, 'x) UniplateRep.t -> 'a -> ('a * 'a UnOp.t) List.t
   (**
    * Returns a list of all substructures of the given value and functions
    * to replace the corresponding substructure in the given value.
    *
    *> map op </ (contexts t x) = universe t x
    *)

   val descend : ('a, 'x) UniplateRep.t -> 'a UnOp.t UnOp.t
   (**
    * Replaces each maximal proper substructure {x} by {f x} in the given
    * value.  This is non-recursive.
    *)

   val para : ('a, 'x) UniplateRep.t -> ('a -> 'b List.t -> 'b) -> 'a -> 'b
   (**
    * A kind of fold.  {para} can be defined as follows:
    *
    *> fun para t f x = f x (map (para t f) (children t x))
    *)

   val rewrite : ('a, 'x) UniplateRep.t -> ('a -> 'a Option.t) -> 'a UnOp.t
   (**
    * Exhaustive recursive bottom-up transformation.  The idea is to keep
    * rewriting as long as some new value is returned.  {rewrite} can be
    * defined as follows:
    *
    *> fun rewrite t f =
    *>     transform t (fn x => case f x
    *>                           of NONE   => x
    *>                            | SOME x => rewrite t f x)
    *)

   val transform : ('a, 'x) UniplateRep.t -> 'a UnOp.t UnOp.t
   (**
    * Recursive bottom-up transformation.  {transform} can be defined as
    * follows:
    *
    *> fun transform t f x = f (descend t (transform t f) x)
    *)

   val uniplate : ('a, 'x) UniplateRep.t -> 'a -> 'a List.t * ('a List.t -> 'a)
   (**
    * Returns a list of all maximal proper substructures (children) of the
    * same type contained in the given value and a function, dubbed
    * context, to replace the substructures.  At immutable contexts, a new
    * value is built.  At mutable contexts, the objects are mutated.  The
    * number of elements in the list given to context must be equal to the
    * number of maximal proper substructure returned.  All functions
    * specified in the {UNIPLATE} signature can be defined in terms of
    * {uniplate}.
    *)
end

signature UNIPLATE_CASES = sig
   include CASES UNIPLATE
   sharing Open.Rep = UniplateRep
end

signature WITH_UNIPLATE_DOM = HASH_CASES
