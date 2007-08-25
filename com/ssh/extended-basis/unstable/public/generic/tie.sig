(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A framework for computing fixpoints.
 *
 * In a strict language you sometimes want to provide a fixpoint
 * combinator for an abstract type {t} to make it possible to write
 * recursive definitions.  Unfortunately, a single combinator {fix} of the
 * type {(t -> t) -> t} does not support mutual recursion.  To support
 * mutual recursion, you would need to provide a family of fixpoint
 * combinators having types of the form {(u -> u) -> u} where {u} is a
 * type of the form {t * ... * t}.  Unfortunately, even such a family of
 * fixpoint combinators does not support mutual recursion over different
 * abstract types.
 *)
signature TIE = sig
   type 'a dom and 'a cod
   type 'a t = 'a dom -> 'a cod
   (**
    * The type of fixpoint witnesses.
    *
    * The type constructors {dom} and {cod} are used to expose the arrow
    * {->} type constructor (to allow eta-expansion) while keeping the
    * domain and codomain abstract.
    *)

   val fix : 'a t -> 'a Fix.t
   (**
    * Produces a fixpoint combinator from the given witness.  For example,
    * one can make a mutually recursive definition of functions:
    *
    *> val isEven & isOdd =
    *>     let open Tie in fix (function *` function) end
    *>        (fn isEven & isOdd =>
    *>            (fn 0 => true
    *>              | 1 => false
    *>              | n => isOdd (n-1)) &
    *>            (fn 0 => false
    *>              | 1 => true
    *>              | n => isEven (n-1)))
    *)

   (** == Making New Witnesses == *)

   val pure : ('a * 'a UnOp.t) Thunk.t -> 'a t
   (**
    * {pure} is a more general version of {tier}.  It is mostly useful for
    * computing fixpoints in a non-imperative manner.
    *)

   val tier : ('a * 'a Effect.t) Thunk.t -> 'a t
   (**
    * {tier} is used to define fixpoint witnesses for new abstract types
    * by providing a thunk whose instantiation allocates a mutable proxy
    * and a procedure for updating it with the result.
    *)

   val id : 'a -> 'a t
   (** {id x} is equivalent to {pure (const (x, id))}. *)

   (** == Combining Existing Witnesses == *)

   val iso : 'b t -> ('a, 'b) Iso.t -> 'a t
   (**
    * Given an isomorphism between {'a} and {'b} and a witness for {'b},
    * produces a witness for {'a}.  This is useful when you have a new
    * type that is isomorphic to some old type for which you already have
    * a witness.
    *)

   val *` : 'a t * 'b t -> ('a, 'b) Product.t t
   (**
    * Given witnesses for {'a} and {'b} produces a witness for the product
    * {('a, 'b) Product.t}.  This is used when mutual recursion is needed.
    *)

   val tuple2 : 'a t * 'b t -> ('a * 'b) t
   (**
    * Given witnesses for {'a} and {'b} produces a witness for the product
    * {'a * 'b}.
    *)

   (** == Particular Witnesses == *)

   val function : ('a -> 'b) t
   (** Witness for functions. *)
end
