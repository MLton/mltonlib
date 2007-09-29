(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic shrinking function.
 *
 * The basic idea is to "shrink" a given value by producing a list of
 * maximal proper (or strict) subvalues (or subsets) of the given value.
 * For example, given a list of booleans, calling {shrink} on the list
 * would produce a list of lists of booleans where each list of booleans
 * is the same as the given list except that it omits one element of the
 * given list.
 *
 * The main application of shrinking is randomized testing.
 *)
signature SHRINK = sig
   structure ShrinkRep : OPEN_REP

   val shrink : ('a, 'x) ShrinkRep.t -> 'a -> 'a List.t
   (** Extracts the single-layer shrinking function. *)

   val shrinkFix : ('a, 'x) ShrinkRep.t -> 'a -> 'a List.t
   (**
    * Shrinks the given value to a fixpoint.
    *
    * WARNING: This function is impractical for most purposes, because the
    * size of the output grows extremely rapidly depending on the type and
    * size of the input.  Frankly, this is mostly provided for playing
    * with in a REPL and might be removed in the future.
    *)
end

signature SHRINK_CASES = sig
   include CASES SHRINK
   sharing Open.Rep = ShrinkRep
end

signature WITH_SHRINK_DOM = sig
   include CASES ORD SIZE
   sharing Open.Rep = OrdRep = SizeRep
end
