(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic linear ordering.
 *
 * The default semantics is an unspecified, structural, linear ordering,
 * suitable for use in applications such as search trees.  The ordering
 * does not necessarily correspond to a "natural" ordering for any type.
 *
 * By default, mutable types (refs and arrays) are ordered structurally
 * and the ordering does not coincide with SML's notion of equality.  More
 * precisely, two mutable object {a} and {b} may compare {EQUAL}, but it
 * is not necessarily the case that {a} and {b} have the same identity.
 * This means that the ordering of mutable objects is not invariant with
 * respect to mutation.
 *
 * By default, comparison of data structures with cycles introduced
 * through references and arrays always terminates with a consistent
 * result.
 *
 * By default, the comparison of reals is done bitwise.  While this
 * matches the default notion of ordering for other types, this differs
 * from the notions of ordering provided for reals by the Basis library.
 * In particular, {~0.0} and {0.0} are considered unequal and {nan} is
 * considered equal to {nan}.  This treatment is important for a number of
 * non-numerical applications such as serialization.
 *
 * By default, comparison of exceptions only works when at least one of
 * the exception constructors involved in a comparison has been
 * registered.
 *
 * Comparison of functions is impossible and fails at run-time.
 *)
signature ORD = sig
   structure OrdRep : OPEN_REP

   val ord : ('a, 'x) OrdRep.t -> 'a Cmp.t
   (** Extracts the linear ordering. *)

   val withOrd : 'a Cmp.t -> ('a, 'x) OrdRep.t UnOp.t
   (** Functionally updates the comparison function. *)
end

signature ORD_CASES = sig
   include CASES ORD
   sharing Open.Rep = OrdRep
end

signature WITH_ORD_DOM = HASH_CASES
