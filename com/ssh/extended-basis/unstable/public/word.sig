(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {WORD} signature. *)
signature WORD = sig
   include WORD

   type t = word
   (** Convenience alias. *)

   (** == Bounds == *)

   val numBytes : Int.t
   (**
    * The number of bytes (8-bit words) it takes to store a {word}.  This
    * is always equal to {(wordSize + 7) quot 8}.
    *)

   val maxWord : t
   (**
    * The maximal representable {word}.  This is always equal to {fromInt
    * ~1}.
    *)

   val minWord : t
   (** The minimal representable {word}.  This is always {0w0}. *)

   val bounds : t Sq.t
   (**
    * Pair of the minimal and maximal representable {word}s.  This is
    * always equal to {(minWord, maxWord)}.
    *)

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

   val fromWord : Word.t -> t
   (**
    * Converts the given word {w : Word.t} to the value {w(mod
    * (2^wordSize))} of type {word}.  This has the effect of taking the
    * low-order {wordSize} bits of the 2's complement representation of
    * {w}.
    *)

   val fromWordX : Word.t -> t
   (**
    * Converts the given word {w : Word.t} to a value of type {word}.
    * {w} is ``sign-extended,'' i.e., the {min (Word.wordSize, wordSize)}
    * low-order bits of {w} and {fromWordX w} are the same, and the
    * remaining bits of {fromWordX w} are all equal to the most
    * significant bit of {w}.
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

   val toWord : t -> Word.t
   (**
    * Converts the given word {w : word} to the value {w(mod
    * (2^Word.wordSize))} of type {Word.t}.  This has the effect of
    * taking the low-order {Word.wordSize} bits of the 2's complement
    * representation of {w}.
    *)

   val toWordX : t -> Word.t
   (**
    * Converts the given word {w : word} to a value of type {Word.t}.
    * {w} is ``sign-extended,'' i.e., the {min (Word.wordSize, wordSize)}
    * low-order bits of {w} and {toWordX w} are the same, and the
    * remaining bits of {toWordX w} are all equal to the most significant
    * bit of {w}.
    *)

   (** == Embeddings == *)

   val embString : (t, String.t) Emb.t
   (**
    * An embedding of words into strings.  It is always equivalent to
    * {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoBigBytes : (t, Word8Vector.t) Iso.t
   (**
    * An isomorphism between words and byte vectors.  It is always
    * equivalent to {(toBigBytes, fromBigBytes)}.
    *)

   val isoInt : (t, Int.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the default integer
    * type.  It is always equivalent to {(toInt, fromInt)}.
    *)

   val isoIntX : (t, Int.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the default integer
    * type.  It is always equivalent to {(toIntX, fromInt)}.
    *)

   val isoLarge : (t, LargeWord.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the {LargeWord.t}
    * type.  It is always equivalent to {(toLarge, fromLarge)}.
    *)

   val isoLargeInt : (t, LargeInt.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the {LargeInt.t}
    * type.  It is always equivalent to {(toLargeInt, fromLargeInt)}.
    *)

   val isoLargeIntX : (t, LargeInt.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the {LargeInt.t}
    * type.  It is always equivalent to {(toLargeIntX, fromLargeInt)}.
    *)

   val isoLargeX : (t, LargeWord.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the {LargeWord.t}
    * type.  It is always equivalent to {(toLargeX, fromLarge)}.
    *)

   val isoLittleBytes : (t, Word8Vector.t) Iso.t
   (**
    * An isomorphism between words and byte vectors.  It is always
    * equivalent to {(toLittleBytes, fromLittleBytes)}.
    *)

   val isoWord : (t, Word.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the default word
    * type.  It is always equivalent to {(toWord, fromWord)}.
    *)

   val isoWordX : (t, Word.t) Iso.t
   (**
    * An isomorphism between words of type {word} and the default word
    * type.  It is always equivalent to {(toWordX, fromWordX)}.
    *)

   (** == Predicates == *)

   val isEven : t -> Bool.t
   (**
    * Returns true if the given word is of the form {0w2*n} for some
    * word {n}.
    *)

   val isOdd : t -> Bool.t
   (**
    * Returns true if the given word is of the form {0w2*n+0w1} for some
    * word {n}.
    *)

   val isZero : t -> Bool.t
   (** Returns true if the given word is {0w0}. *)
end
