(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
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
 *
 * See also: http://mlton.org/Fixpoints
 *)
signature TIE = sig
   type 'a t_dom and 'a t_cod
   type 'a t = 'a t_dom -> 'a t_cod
   (**
    * The type of fixpoint tiers.
    *
    * The type constructors {t_dom} and {t_cod} are used to expose the
    * arrow {->} type constructor (to allow eta-expansion) while
    * preventing clients from actually applying tiers.
    *)

   val fix : 'a t -> 'a Fix.t
   (**
    * Produces a fixpoint combinator from the given tier.  For example,
    * given a module {Fn} implementing a tier {Fn.Y} for functions, one
    * could make a mutually recursive definition of functions:
    *
    *> val isEven & isOdd =
    *>     let open Tie in fix (Fn *` Fn) end
    *>        (fn isEven & isOdd =>
    *>            (fn 0w0 => true
    *>              | 0w1 => false
    *>              | n => isOdd (n-0w1)) &
    *>            (fn 0w0 => false
    *>              | 0w1 => true
    *>              | n => isEven (n-0w1)))
    *)

   val pure : ('a * 'a UnOp.t) Thunk.t -> 'a t
   (**
    * {pure} is a more general version of {tier}.  It is mostly useful for
    * computing fixpoints in a non-imperative manner.
    *)

   val tier : ('a * 'a Effect.t) Thunk.t -> 'a t
   (**
    * {tier} is used to define fixpoint tiers for new abstract types by
    * providing a thunk whose instantiation allocates a fresh "knot" and a
    * procedure for "tying" it.
    *)

   val iso : 'b t -> ('a, 'b) Iso.t -> 'a t
   (**
    * Given an isomorphism between {'a} and {'b} and a tier for {'b},
    * produces a tier for {'a}.  This is useful when you have a new type
    * that is isomorphic to some old type for which you already have a
    * tier.
    *)

   val *` : 'a t * 'b t -> ('a, 'b) Product.t t
   (**
    * Given tiers for {'a} and {'b} produces a tier for the product {('a,
    * 'b) Product.t}.  This is used when mutual recursion is needed.
    *)

   val tuple2 : 'a t * 'b t -> ('a * 'b) t
   (**
    * Given tiers for {'a} and {'b} produces a tier for the product {'a *
    * 'b}.
    *)

   val option : 'a Option.t t
   (** Tier for options. *)

   val function : ('a -> 'b) t
   (** Tier for functions. *)
end
