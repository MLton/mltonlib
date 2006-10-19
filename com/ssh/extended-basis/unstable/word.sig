(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {WORD} signature.
 *)

signature WORD =
   sig
      include WORD
      val toWord : word -> Word.word
      val fromWord : Word.word -> word
      val intIso : (word, Int.int) iso
      val intXIso : (word, Int.int) iso
      val largeIso : (word, LargeWord.word) iso
      val largeIntIso : (word, LargeInt.int) iso
      val largeIntXIso : (word, LargeInt.int) iso
      val largeXIso : (word, LargeWord.word) iso
      val wordIso : (word, Word.word) iso
      val stringEmb : (word, string) emb
      val isZero : word -> bool
      val isEven : word -> bool
      val isOdd : word -> bool
      val minWord : word
      val maxWord : word
      val bounds : word * word
   end
