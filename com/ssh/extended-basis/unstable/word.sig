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
      val int : (word, Int.int) iso
      val intX : (word, Int.int) iso
      val large : (word, LargeWord.word) iso
      val largeInt : (word, LargeInt.int) iso
      val largeIntX : (word, LargeInt.int) iso
      val largeX : (word, LargeWord.word) iso
      val word : (word, Word.word) iso
      val string : (word, string) emb
      val is0 : word -> bool
      val isEven : word -> bool
      val isOdd : word -> bool
      val minWord : word
      val maxWord : word
      val bounds : word * word
   end
