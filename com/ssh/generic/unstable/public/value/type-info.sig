(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a type-indexed family of type properties.
 *
 * These type properties can be useful for both optimizations and for
 * ensuring correctness.  As an optimization one could, for example,
 * determine whether one needs to handle cyclic values (which can be
 * costly) or not.  As a correctness issue, one can avoid generating
 * infinite data structures or avoid performing non-terminating operations
 * on infinite data structures.
 *
 * This type-indexed function is unlikely to be directly useful in
 * application programs and is more likely to be used internally in the
 * implementation of some other type-indexed functions (e.g. pickling).
 *)
signature TYPE_INFO = sig
   structure TypeInfo : EXT_GENERIC_INDEX

   val canBeCyclic : ('a, 'x) TypeInfo.t UnPr.t
   (**
    * Returns true iff {'a} is of the form {'b ref} or {'b array} and
    * it can not be ruled out that values of the type can form cycles.
    *
    * Note: Functions are not considered to form cycles.
    *)

   val hasBaseCase : ('a, 'x) TypeInfo.s UnPr.t
   (** Returns true iff the type {'a} has a non-recursive variant. *)

   val hasExn : ('a, 'x) TypeInfo.t UnPr.t
   (** Returns true iff the type {'a} contains the type {exn}. *)

   val hasRecData : ('a, 'x) TypeInfo.t UnPr.t
   (**
    * Returns true iff the type {'a} contains recursive references to
    * datatypes.
    *)

   val isRefOrArray : ('a, 'x) TypeInfo.t UnPr.t
   (**
    * Returns true iff the type {'a} is of the form {'b array} or of
    * the form {'b ref}.
    *)

   val numConsecutiveAlts : ('a, 'x) TypeInfo.s -> Int.t
   (**
    * Number of consecutive alternatives.
    *)
end

signature TYPE_INFO_GENERIC = sig
   include TYPE_INFO EXT_GENERIC
   sharing TypeInfo = Index
end
