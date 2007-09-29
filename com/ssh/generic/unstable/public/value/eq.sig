(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic equality predicate.
 *
 * By default, for equality types the semantics is the same as SML's
 * equality.  For mutable types (refs and arrays) this means that two
 * objects are considered equal iff they have the same identity.  This
 * means that the result of comparing two particular mutable objects is
 * invariant.  If you need a structural equality relation for mutable
 * types that merely requires objects to have one-to-one sharing, see
 * {SEQ}.  If you truly need a structural equality relation for mutable
 * types that ignores identity, see {ORD}.
 *
 * By default, comparison of data structures with cycles introduced
 * through refs and arrays always terminates with a consistent result.
 *
 * By default, the comparison of reals is done bitwise.  While this
 * matches the default notion of equality provided for other types, this
 * differs from the notions of equality provided for reals by the Basis
 * Library.  In particular, {~0.0} and {0.0} are considered unequal and
 * {nan} is considered equal to {nan}.  This treatment is important for a
 * number of non-numerical applications such as serialization.
 *
 * By default, user defined datatypes and exceptions are given a
 * structural semantics of equality.  Specifically, two datatype or
 * exception values are considered equal iff they have the same
 * constructor and the arguments of the constructors are considered equal.
 * Of course, all of this is modulo user specified morphisms!
 *
 * By default, comparison of exceptions only works when at least one of
 * the exception constructors involved in a comparison has been
 * registered.
 *
 * Comparison of functions is impossible and fails at run-time.
 *)
signature EQ = sig
   structure EqRep : OPEN_REP

   val eq : ('a, 'x) EqRep.t -> 'a BinPr.t
   (** Extracts the equality predicate. *)

   val notEq : ('a, 'x) EqRep.t -> 'a BinPr.t
   (** {notEq t = not o eq t} *)

   val withEq : 'a BinPr.t -> ('a, 'x) EqRep.t UnOp.t
   (**
    * Functionally updates the equality predicate.
    *
    * Note that specializing equality often means that several other
    * generic functions need to be specialized as well.  In particular,
    * generic hashing and ordering should generally respect equality.
    * Instead of specializing equality, consider using a canonic data
    * representation where each equivalence class is a singleton.
    *)
end

signature EQ_CASES = sig
   include CASES EQ
   sharing Open.Rep = EqRep
end

signature WITH_EQ_DOM = CASES
