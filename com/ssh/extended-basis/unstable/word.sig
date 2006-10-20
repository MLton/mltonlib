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
   val toWord : word -> Word.word

   (** == Embeddings == *)

   val stringEmb : (word, string) emb

   (** == Isomorphisms == *)

   val intIso : (word, Int.int) iso
   val intXIso : (word, Int.int) iso
   val largeIntIso : (word, LargeInt.int) iso
   val largeIntXIso : (word, LargeInt.int) iso
   val largeIso : (word, LargeWord.word) iso
   val largeXIso : (word, LargeWord.word) iso
   val wordIso : (word, Word.word) iso

   (** == Predicates == *)

   val isEven : word -> bool
   val isOdd : word -> bool
   val isZero : word -> bool
end
