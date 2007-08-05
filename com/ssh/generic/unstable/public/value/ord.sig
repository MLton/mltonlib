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
 * By default, the comparison of reals is done bitwise.  While this
 * matches the notion of ordering for other types, this differs from the
 * notions of ordering provided for reals by the Basis library.  In
 * particular, {~0.0} and {0.0} are considered unequal and {nan} is
 * considered equal to {nan}.  This treatment is important for a number of
 * non-numerical applications such as serialization.
 *
 * By default, comparison of exceptions only works when at least one of
 * the exception constructors involved in a comparison has been registered
 * with {regExn}.
 *
 * Comparison of functions is impossible and fails at run-time.
 *)
signature ORD = sig
   structure Ord : OPEN_GENERIC_REP

   val ord : ('a, 'x) Ord.t -> 'a Cmp.t
   (** Extracts the linear ordering. *)

   val withOrd : 'a Cmp.t -> ('a, 'x) Ord.t UnOp.t
   (** Functionally updates the comparison function. *)
end

signature ORD_GENERIC = sig
   include OPEN_GENERIC ORD
   sharing Rep = Ord
end
