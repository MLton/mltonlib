(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic pickle/unpickle function.
 *
 * WARNING: The pickle format contains the {typeHash} of the pickled type.
 * While this can help to detect accidental type mismatches (pickling with
 * one type and then unpickling with another) it is not fool proof nor
 * designed to be secure in any way.
 *
 * The pickle format is designed to be platform independent.  For example,
 * it is possible to pickle on a 32-bit big-endian platform and unpickle
 * on a 64-bit little-endian platform or vice-versa.  Types whose sizes
 * are platform dependent use variable length or explicit precision
 * encodings.  Unpickling fails if an encoded value is not representable
 * or there is no conversion from the pickled precision.
 *
 * The pickle format is byte-oriented (not bit-oriented) and relatively
 * compact given the platform independency.  Entropy coding is likely to
 * be effective on pickled data, because tags in pickled data are biased
 * towards small octets.  The pickle format should admit relatively
 * efficient pickling and unpickling, especially given a few reasonable
 * primitives, but the current implementation is not written for
 * efficiency.  Sharing is only introduced if it possibly decreases the
 * size of pickles or is required due to mutable data structures.
 *)
signature PICKLE = sig
   structure Pickle : OPEN_REP

   structure Pickling : sig
      exception TypeMismatch
      (** Raised by unpickling functions when a type-mismatch is detected. *)
   end

   (** == Stream Interface ==
    *
    * The {pickler} and {unpickler} functions support pickling directly to
    * and unpickling directly from an arbitrary stream without storing the
    * pickle in memory as a whole.
    *)

   val pickler   : ('a, 'x) Pickle.t -> (Char.t -> (Unit.t, 's) IOSMonad.t)
                                     -> ('a     -> (Unit.t, 's) IOSMonad.t)
   val unpickler : ('a, 'x) Pickle.t -> (Char.t, 's) IOSMonad.t
                                     -> ('a,     's) IOSMonad.t

   (** == Simplified Interface ==
    *
    * The {pickle} and {unpickle} functions provide a simplified interface
    * for pickling to strings and unpickling from strings.
    *)

   val pickle   : ('a, 'x) Pickle.t -> 'a -> String.t
   val unpickle : ('a, 'x) Pickle.t -> String.t -> 'a
end

signature PICKLE_CASES = sig
   include OPEN_CASES PICKLE
   sharing Rep = Pickle
end

signature WITH_PICKLE_DOM = sig
   include OPEN_CASES DATA_REC_INFO EQ HASH SOME TYPE_HASH TYPE_INFO
   sharing Rep = DataRecInfo = Eq = Hash = Some = TypeHash = TypeInfo
end
