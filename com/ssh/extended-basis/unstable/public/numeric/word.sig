(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {WORD} signature. *)
signature WORD = sig
   eqtype word

   type t
   (** Convenience alias. *)

   (** == Numeric == *)

   val + : t BinOp.t
   val - : t BinOp.t
   val * : t BinOp.t

   val div : t BinOp.t
   val mod : t BinOp.t

   val ~ : t UnOp.t

   val sq : t UnOp.t (** {sq x = x * x} *)

   (** == Bounds == *)

   val wordSize : Int.int

   val numBytes : Int.t
   (**
    * The number of bytes (8-bit words) it takes to store a {word}.  This
    * is always equal to {(wordSize + 7) quot 8}.
    *)

   val largestPrime : t
   (** Largest prime that fits in the word. *)

   (** == Conversions == *)

   val fromBigBytes : Word8Vector.t -> t
   (**
    * Converts a vector of bytes in big-endian order to a word.  Raises
    * {Subscript} if the length of the given vector is not equal to
    * {numBytes}.  Extra bits, if any, in the most significant byte are
    * ignored.
    *)

   val fromLittleBytes : Word8Vector.t -> t
   (**
    * Converts a vector of bytes in little-endian order to a word.  Raises
    * {Subscript} if the length of the given vector is not equal to
    * {numBytes}.  Extra bits, if any, in the most significant byte are
    * ignored.
    *)

   val toBigBytes : t -> Word8Vector.t
   (**
    * Converts the given word to a vector of bytes in big-endian order.
    * Extra bits, if any, in the most significant byte will be set to
    * zeroes.
    *)

   val toLittleBytes : t -> Word8Vector.t
   (**
    * Converts the given word to a vector of bytes in little-endian order.
    * Extra bits, if any, in the most significant byte will be set to
    * zeroes.
    *)

   (** == Isomorphisms == *)

   val isoBigBytes : (t, Word8Vector.t) Iso.t
   (**
    * An isomorphism between words and byte vectors.  It is always
    * equivalent to {(toBigBytes, fromBigBytes)}.
    *)

   val isoLittleBytes : (t, Word8Vector.t) Iso.t
   (**
    * An isomorphism between words and byte vectors.  It is always
    * equivalent to {(toLittleBytes, fromLittleBytes)}.
    *)

   (** == Predicates == *)

   val isEven : t UnPr.t
   (**
    * Returns true if the given word is of the form {0w2*n} for some
    * word {n}.
    *)

   val isOdd : t UnPr.t
   (**
    * Returns true if the given word is of the form {0w2*n+0w1} for some
    * word {n}.
    *)

   val isZero : t UnPr.t
   (** Returns true if the given word is {0w0}. *)

   (** == Concepts == *)

   include BITWISE
   include BOUNDED
   include FORMATTABLE_and_SCANNABLE_FROM_FORMAT
           where type formattable_format = BasisStringCvt.radix
   include INTABLE_X
   include LARGEABLE_X where type largeable_large = LargeWord.t
   include ORDERED
   include SHIFTABLE_FIN
   include STRINGABLE
   include WORDABLE_X

   sharing type bitwise = bounded = formattable = intable = largeable = ordered
              = shiftable = stringable = t = word = wordable
end
