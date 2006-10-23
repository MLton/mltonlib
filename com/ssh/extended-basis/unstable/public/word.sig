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
   val minWord : word

   val bounds : word * word

   (** == Conversions == *)

   val fromWord : Word.word -> word
   val fromWordX : Word.word -> word

   val toWord : word -> Word.word
   val toWordX : word -> Word.word

   (** == Embeddings == *)

   val embString : (word, string) emb

   (** == Isomorphisms == *)

   val isoInt : (word, Int.int) iso
   val isoIntX : (word, Int.int) iso
   val isoLarge : (word, LargeWord.word) iso
   val isoLargeInt : (word, LargeInt.int) iso
   val isoLargeIntX : (word, LargeInt.int) iso
   val isoLargeX : (word, LargeWord.word) iso
   val isoWord : (word, Word.word) iso
   val isoWordX : (word, Word.word) iso

   (** == Predicates == *)

   val isEven : word -> bool
   val isOdd : word -> bool
   val isZero : word -> bool
end
