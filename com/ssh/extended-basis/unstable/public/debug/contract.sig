(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * The {Contract} module provides a combinator library for specifying
 * contracts as in Design by Contract.
 *
 * As an example, suppose we have a (naively implemented) sqrt function
 *
 *> fun sqrt x =
 *>     recur 0 (fn lp =>
 *>        fn y =>
 *>           if x < sq (y+1) then y else lp (y+1))
 *
 * where
 *
 *> fun sq x = x * x
 *
 * A contract for the function could be specified as
 *
 *> val sqrtContract =
 *>     pr (fn x => 0 <= x) --> (fn x =>
 *>     pr (fn y => 0 <= y andalso sq y = x))
 *
 * where the first {pr (fn ...)} expression specifies the contract for the
 * domain and the second {pr (fn ...)} expression specifies the contract
 * for the range of a function.  Note that the contract for the range
 * depends on the value of the domain for the function invocation.
 *
 * Now, a "contracted" version of the function could be implemented as
 *
 *> val csqrt = assert sqrtContract sqrt
 *
 * Calling the {csqrt} function as {csqrt 4} returns {2}.  OTOH, calling
 * {csqrt ~4} raises the exception {Caller Contract}, which suggests that
 * the caller broke the contract.  Also, calling {csqrt 5} raises {Callee
 * Contract}, which suggests that the callee broke the contract.  In this
 * case, however, it is actually the contract that we got wrong.  A
 * correct contract for sqrt could be specified as:
 *
 *> val sqrtContract =
 *>     pr (fn x => 0 <= x) --> (fn x =>
 *>     pr (fn y => 0 <= y andalso sq y <= x andalso x < sq (y+1)))
 *
 * With the corrected contract, {csqrt 5} returns {2} as expected.
 *
 * Contracts can be treated as first-class values and thanks to the arrow
 * contract constructor, {-->}, contracts generalize to higher-order
 * functions.
 *
 * Inspiration for the {Contract} module comes mainly from the article:
 *
 *   Contracts for Higher-Order Functions
 *   Robert Bruce Findler and Matthias Felleisen
 *   ICFP 2002
 *   [http://citeseer.ist.psu.edu/findler02contracts.html]
 *
 * Another combinator library with the same source of inspiration, but a
 * GADT based implementation, is described in the article:
 *
 *   Typed Contracts for Functional Programming
 *   Ralf Hinze, Johan Jeuring, and Andres Löh
 *   FLOPS 2006
 *   [http://people.cs.uu.nl/andres/Contracts.html]
 *)
signature CONTRACT = sig
   type 'a t
   (** The type constructor of contracts. *)

   exception Contract
   (** Raised by {pr} when the given predicate is not satisfied. *)

   exception Caller of Exn.t and Callee of Exn.t
   (**
    * The arrow combinator {-->} tags any raised exception with either
    * {Caller} or {Callee} to indicate where the blame for the contract
    * violation should be assigned.
    *)

   val assert : 'a t -> 'a UnOp.t
   (**
    * Applies the contract to the given value, returning a (possibly) new
    * value.  Higher-order values are wrapped with a contract checker that
    * checks the contract dynamically.
    *)

   val ef : 'a Effect.t -> 'a t
   (**
    * Lifts an effect to a contract.  The intention is that the effect
    * examines, but does not modify, any given value and raises an
    * exception if the value does not satisfy the desired contract.
    *)

   val pr : 'a UnPr.t -> 'a t
   (**
    * Lifts a predicate to a contract.  The contract raises {Contract} in
    * case a value does not satisfy the predicate.
    *)

   val any : 'a t
   (**
    * Contract that is satisfied by any value.  {any} is equivalent to {pr
    * (const true)}.
    *)

   val none : 'a t
   (**
    * Contract that is not satisfied by any value.  {none} is equivalent
    * to {pr (const false)}.
    *)

   val --> : 'a t * ('a -> 'b t) -> ('a -> 'b) t
   (**
    * Given a contract for the domain of type {'a} and a contract
    * constructor for the range of type {'b}, returns a contract for a
    * function of type {'a -> 'b}.
    *
    * The contract constructor for the range is given the value of the
    * domain being passed to the function.  Furthermore, it is guaranteed
    * that the contract for the range is constructed before the contracted
    * function is called.
    *)

   val andAlso : 'a t BinOp.t
   (**
    * Conjunction of contracts.  {a andAlso b} is a contract that is
    * satisfied iff both {a} and {b} are satisfied.
    *
    * To understand the interaction of {-->} and {andAlso}, consider the
    * following example:
    *
    *> fun say ms = ef (fn () => prints ms)
    *> fun con i = say ["D", i] --> (fn () => (prints ["C", i] ; say ["R", i]))
    *
    *> val () = assert (con "1" andAlso con "2") (fn () => ()) ()
    *
    * The output from the example is:
    *
    *> D2C2D1C1R1R2
    *)
end
