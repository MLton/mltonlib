(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a generic pickle/unpickle function.
 *
 * Example:
 *
 *> - val t = tuple2 (largeInt, list order) ;
 *> val t = - : (IntInf.t * Order.t List.t) Rep.t
 *> - val p = pickle t (3141592653589793238, [LESS, EQUAL, GREATER]) ;
 *> val p = "\183\^N\1873\^@\b\214I2\162\223-\153+\^@\^C\^@\^A\^B"
 *>   : String.t
 *> - size p ;
 *> val it = 19 : Int.t
 *> - val x = unpickle t p ;
 *> val x = (3141592653589793238, [LESS, EQUAL, GREATER])
 *>   : IntInf.t * Order.t List.t
 *
 * == About the Design and Implementation ==
 *
 * The pickle format is designed to be platform independent.  For example,
 * it is possible to pickle on a 32-bit big-endian platform and unpickle
 * on a 64-bit little-endian platform or vice-versa.  Types whose sizes
 * are platform dependent use variable length or explicit precision
 * encodings.  Unpickling fails if an encoded value is not representable
 * (e.g. raises {Overflow}) or there is no conversion from the pickled
 * precision (e.g. pickle contains 80-bit reals, but only 64-bit reals are
 * supported on the host).
 *
 * The pickle format is byte-oriented (not bit-oriented) and relatively
 * compact given the platform independency.  Entropy coding is likely to
 * be effective on pickled data, because tags in pickled data are biased
 * towards small octets.  The pickle format should admit relatively
 * efficient pickling and unpickling, especially given a few reasonable
 * primitives, but the current implementation is not written for
 * efficiency.
 *
 * Cyclic data structures are supported and observable sharing through
 * mutable types, refs and arrays, is respected.  In other words,
 * unpickling reconstructs the cycles and sharing present in the object
 * that was pickled.
 *
 * As an interesting statistic, the pickling generic uses no less than 6
 * other generics:
 *
 * {DataRecInfo}
 *   is used to perform a simple data recursion analysis, which allows the
 *   pickling generic to automatically perform a (minor) optimization
 *   similar to the {ref0} combinator described in [5].
 *
 * {Eq} and {Hash}
 *   are used in the implementation of sharing (and cycle reconstruction).
 *
 * {Some}
 *   is used to generate dummy values for building cyclic data structures.
 *   This eliminates the need to pass a dummy value as an argument like in
 *   [5].
 *
 * {TypeHash}
 *   computes a type-representation specific hash.  The produced pickle
 *   contains the hash and unpickling raises {TypeMismatch} if the hash
 *   value does not match.
 *
 *   Note that while this may help to detect accidental type mismatches
 *   (pickling with one type and then unpickling with another) it is
 *   neither fool proof nor designed to be secure in any way.
 *
 * {TypeInfo}
 *   helps with computing integer indices (0, 1, 2, ...) for constructors.
 *   The upshot is that the user does not need to explicitly provide an
 *   isomorphism between integers and the constructors of a datatype like
 *   in [5] and [9].
 *
 * The pickling generic introduces sharing automatically.  Sharing is only
 * introduced if the estimated size of the pickle is over a threshold (or
 * is required due to mutability).  This means that values of small types
 * (like chars) are automatically not shared (it would be inefficient in
 * both time and space) and "double sharing" is avoided.  This also means
 * that maximal sharing may not be obtained, but it couldn't be observed
 * anyway, smart garbage collectors (e.g. MLton's) can introduce sharing
 * automatically (and probably more efficiently), the potential for
 * sharing depends on the internal data representation chosen by the
 * compiler (a smart compiler eliminates unnecessary indirections from
 * data, which reduces potential for sharing), and to achieve maximal
 * sharing, efficiency would have to be sacrificed.
 *
 * The pickling generic automatically chooses a compact format for
 * pickling constructors based on the knowledge of how many constructors a
 * datatype has.  If a datatype contains only a single constructor, no tag
 * is written at all (0-bits of storage).  If a datatype contains a
 * maximum of 256 constructors, the tag takes 8-bits.  Otherwise a
 * variable length tag is used.
 *
 * For maximal flexibility, the interface allows pickling / unpickling
 * directly to / from an arbitrary stream.
 *
 * Currently the pickling generic does not support anything like the
 * (unsafe) {refLin} combinator described in [5].  It would not be
 * difficult to support such a feature by adding a combinator with a spec
 * of the following form:
 *
 *> val notShared : ('a Ref.t, 'x) Pickle.t UnOp.t
 *
 * If you really need it (due to efficiency), let us know.
 *)
signature PICKLE = sig
   structure PickleRep : OPEN_REP

   structure Pickle : sig
      exception TypeMismatch
      (** Raised by unpickling functions when a type-mismatch is detected. *)
   end

   (** == Stream Interface ==
    *
    * The {pickler} and {unpickler} functions support pickling directly to
    * and unpickling directly from an arbitrary stream without storing the
    * pickle in memory as a whole.
    *)

   val pickler   : ('a, 'x) PickleRep.t -> (Char.t -> (Unit.t, 's) IOSMonad.t)
                                        -> ('a     -> (Unit.t, 's) IOSMonad.t)
   val unpickler : ('a, 'x) PickleRep.t -> (Char.t, 's) IOSMonad.t
                                        -> ('a,     's) IOSMonad.t

   (** == Simplified Interface ==
    *
    * The {pickle} and {unpickle} functions provide a simplified interface
    * for pickling to strings and unpickling from strings.
    *)

   val pickle   : ('a, 'x) PickleRep.t -> 'a -> String.t
   val unpickle : ('a, 'x) PickleRep.t -> String.t -> 'a
end

signature PICKLE_CASES = sig
   structure Open : OPEN_CASES
   include PICKLE
   sharing Open.Rep = PickleRep
end

signature WITH_PICKLE_DOM = sig
   structure Open : OPEN_CASES
   include DATA_REC_INFO EQ HASH SOME TYPE_HASH TYPE_INFO
   sharing Open.Rep = DataRecInfoRep = EqRep = HashRep = SomeRep = TypeHashRep
         = TypeInfoRep
end
