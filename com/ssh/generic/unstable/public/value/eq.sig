(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic equality relation.
 *
 * For equality types the semantics is the same as SML's equality.  For
 * mutable types (refs and arrays) this means that two objects are
 * considered equal iff they have the same identity.  User defined
 * datatypes and exceptions are given a structural semantics of equality.
 * Specifically, two datatypes or exceptions are considered equal iff they
 * have the same constructor and the arguments of the constructors are
 * considered equal.  (Of course, all of this is modulo user specified
 * morphisms.)
 *
 * The equality of reals is bitwise equality.  While this matches the
 * notion of equality provided for other types, this differs from the
 * notions of equality provided for reals by the Basis Library.  In
 * particular, {~0.0} and {0.0} are considered unequal and {nan} is
 * considered equal to {nan}.  This treatment of equality is important for
 * a number of non-numerical applications such as serialization.
 *)
signature EQ = sig
   structure Eq : OPEN_GENERIC_REP

   val eq : ('a, 'x) Eq.t -> 'a BinPr.t
   (**
    * Extracts the equality relation.  Note that the type variable {'a}
    * isn't an equality type variable.
    *)

   val notEq : ('a, 'x) Eq.t -> 'a BinPr.t
   (** {notEq t = not o eq t} *)
end

signature EQ_GENERIC = sig
   include OPEN_GENERIC EQ
   sharing Rep = Eq
end
