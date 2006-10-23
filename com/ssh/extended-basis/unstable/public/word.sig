(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {WORD} signature.
 *)
signature WORD = sig
   include WORD

   (** == Bounds == *)

   val maxWord : word
   (**
    * The maximal representable {word}.  This is always equal to {fromInt
    * ~1}.
    *)

   val minWord : word
   (**
    * The minimal representable {word}.  This is always {0w0}.
    *)

   val bounds : word * word
   (**
    * Pair of the minimal and maximal representable {word}s.  This is
    * always equal to {(minWord, maxWord)}.
    *)

   (** == Conversions == *)

   val fromWord : Word.word -> word
   (**
    * Converts the given word {w : Word.word} to the value {w(mod
    * (2^wordSize))} of type {word}.  This has the effect of taking the
    * low-order {wordSize} bits of the 2's complement representation of
    * {w}.
    *)

   val fromWordX : Word.word -> word
   (**
    * Converts the given word {w : Word.word} to a value of type {word}.
    * {w} is ``sign-extended,'' i.e., the {min (Word.wordSize, wordSize)}
    * low-order bits of {w} and {fromWordX w} are the same, and the
    * remaining bits of {fromWordX w} are all equal to the most
    * significant bit of {w}.
    *)

   val toWord : word -> Word.word
   (**
    * Converts the given word {w : word} to the value {w(mod
    * (2^Word.wordSize))} of type {Word.word}.  This has the effect of
    * taking the low-order {Word.wordSize} bits of the 2's complement
    * representation of {w}.
    *)

   val toWordX : word -> Word.word
   (**
    * Converts the given word {w : word} to a value of type {Word.word}.
    * {w} is ``sign-extended,'' i.e., the {min (Word.wordSize, wordSize)}
    * low-order bits of {w} and {toWordX w} are the same, and the
    * remaining bits of {toWordX w} are all equal to the most significant
    * bit of {w}.
    *)

   (** == Embeddings == *)

   val embString : (word, string) emb
   (**
    * An embedding of words into strings.  It is always equivalent to
    * {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : (word, Int.int) iso
   (**
    * An isomorphism between words of type {word} and the default integer
    * type.  It is always equivalent to {(toInt, fromInt)}.
    *)

   val isoIntX : (word, Int.int) iso
   (**
    * An isomorphism between words of type {word} and the default integer
    * type.  It is always equivalent to {(toIntX, fromInt)}.
    *)

   val isoLarge : (word, LargeWord.word) iso
   (**
    * An isomorphism between words of type {word} and the {LargeWord.word}
    * type.  It is always equivalent to {(toLarge, fromLarge)}.
    *)

   val isoLargeInt : (word, LargeInt.int) iso
   (**
    * An isomorphism between words of type {word} and the {LargeInt.int}
    * type.  It is always equivalent to {(toLargeInt, fromLargeInt)}.
    *)

   val isoLargeIntX : (word, LargeInt.int) iso
   (**
    * An isomorphism between words of type {word} and the {LargeInt.int}
    * type.  It is always equivalent to {(toLargeIntX, fromLargeInt)}.
    *)

   val isoLargeX : (word, LargeWord.word) iso
   (**
    * An isomorphism between words of type {word} and the {LargeWord.word}
    * type.  It is always equivalent to {(toLargeX, fromLarge)}.
    *)

   val isoWord : (word, Word.word) iso
   (**
    * An isomorphism between words of type {word} and the default word
    * type.  It is always equivalent to {(toWord, fromWord)}.
    *)

   val isoWordX : (word, Word.word) iso
   (**
    * An isomorphism between words of type {word} and the default word
    * type.  It is always equivalent to {(toWordX, fromWordX)}.
    *)

   (** == Predicates == *)

   val isEven : word -> bool
   (**
    * Returns true if the given word is of the form {0w2*n} for some
    * word {n}.
    *)

   val isOdd : word -> bool
   (**
    * Returns true if the given word is of the form {0w2*n+0w1} for some
    * word {n}.
    *)

   val isZero : word -> bool
   (**
    * Returns true if the given word is {0w0}.
    *)
end
