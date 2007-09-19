(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for generic type properties.
 *
 * These type properties can be useful for both optimizations and for
 * ensuring correctness.  Using {numAlts} and {numElems} one can balance
 * resources across sums and products.  Using {hasBaseCase}, one can avoid
 * generating infinite data structures or avoid performing non-terminating
 * operations on infinite data structures.
 *
 * This generic value is unlikely to be directly useful in application
 * programs and is more likely to be used internally in the implementation
 * of some other generics (e.g. hashing).
 *)
signature TYPE_INFO = sig
   structure TypeInfoRep : OPEN_REP

   (** == Sums == *)

   val hasBaseCase : ('a, 'x) TypeInfoRep.s UnPr.t
   (** Returns true iff the type {'a} has a non-recursive variant. *)

   val numAlts : ('a, 'x) TypeInfoRep.s -> Int.t
   (** Number of alternatives in the given incomplete sum. *)

   (** == Products == *)

   val numElems : ('a, 'k, 'x) TypeInfoRep.p -> Int.t
   (** Number of elements in the given incomplete product. *)
end

signature TYPE_INFO_CASES = sig
   include OPEN_CASES TYPE_INFO
   sharing Rep = TypeInfoRep
end
