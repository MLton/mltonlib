(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a type-indexed equality relation.  For equality types the
 * semantics is the same as SML's built-in equality.  User defined types,
 * exceptions, and reals are given a natural, structural, semantics of
 * equality.  Functions, obviously, can't be supported.
 *)
signature EQ = sig
   structure Eq : OPEN_GENERIC_REP

   val eq : ('a, 'x) Eq.t -> 'a BinPr.t
   (**
    * Extracs the equality relation.  Note that the type parameter {'a}
    * isn't an equality type variable.
    *)

   val notEq : ('a, 'x) Eq.t -> 'a BinPr.t
   (** {notEq t = not o eq t} *)
end

signature EQ_GENERIC = sig
   include EQ OPEN_GENERIC
   sharing Eq = Rep
end
