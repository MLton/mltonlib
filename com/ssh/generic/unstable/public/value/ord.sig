(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a type-indexed family of compare functions.  The idea is
 * that the compare functions just implement some arbitrary logical
 * ordering that you need for things such as search trees.
 *
 * Note that comparison of functions is impossible and fails at run-time.
 * Comparison of exceptions only works when both exception constructors
 * involved in a comparison have been registered with {regExn}.  Also,
 * comparison of arrays and references does not coincide with SML's notion
 * of equality.  More precisely, for an implementation of the {ORD}
 * signature, two arrays (or refs) {a} and {b} may compare {EQUAL}, but it
 * is not necessarily the case that {a=b} evaluates to {true}.
 *)
signature ORD = sig
   structure Ord : OPEN_GENERIC_REP

   val compare : ('a, 'x) Ord.t -> 'a Cmp.t
   (** Extracts the compare function. *)
end

signature ORD_GENERIC = sig
   include ORD OPEN_GENERIC
   sharing Ord = Rep
end
