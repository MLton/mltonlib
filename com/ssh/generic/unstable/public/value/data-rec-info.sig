(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic datatype recursion analysis.
 *
 * In Standard ML, cyclic data structures, ignoring closures, can only be
 * implemented through mutable types, references and arrays.  Furthermore,
 * a mutable type can only be used to form cycles if it is a part of a
 * strongly connected component containing a recursive datatype or
 * contains exceptions.  This makes it possible to compute a simple
 * conservative approximation as to whether a given mutable type can be
 * part of cycle.
 *
 * These type properties can be useful for both optimizations and for
 * ensuring correctness.  As an optimization one could, for example,
 * determine whether one needs to handle cyclic values (which can be
 * costly) or not.
 *
 * This generic value is unlikely to be directly useful in application
 * programs and is more likely to be used internally in the implementation
 * of some other generics (e.g. pickling).
 *)
signature DATA_REC_INFO = sig
   structure DataRecInfoRep : OPEN_REP

   val mayBeCyclic : ('a, 'x) DataRecInfoRep.t UnPr.t
   (**
    * Returns true if {'a} is a mutable type and may be part of a
    * recursive datatype or contain exceptions.  This means that values of
    * the type can form cycles.
    *)

   val mayContainExn : ('a, 'x) DataRecInfoRep.t UnPr.t
   (**
    * Returns true if a value of the type {'a} may contain exceptions.
    * Arrow types are not considered to contain exceptions.
    *)

   val mayBeRecData : ('a, 'x) DataRecInfoRep.t UnPr.t
   (**
    * Returns true if a value of type {'a} may be part of a recursive
    * datatype.  Exceptions are not considered to be a recursive datatype
    * and arrow types are not considered to contain recursive datatypes.
    *)

   val isMutableType : ('a, 'x) DataRecInfoRep.t UnPr.t
   (**
    * Returns true iff the type {'a} is of the form {'b Array.t} or of the
    * form {'b Ref.t}.
    *)
end

signature DATA_REC_INFO_CASES = sig
   include CASES DATA_REC_INFO
   sharing Open.Rep = DataRecInfoRep
end

signature WITH_DATA_REC_INFO_DOM = CASES
