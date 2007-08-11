(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Wordable ==
 *
 * Wordables can be converted to words and back.
 *)

signature WORDABLE = sig
   type wordable
   val fromLargeWord : LargeWord.t -> wordable
   val fromWord : Word.t -> wordable
   val fromWord8 : Word8.t -> wordable
   val isoLargeWord : (wordable, LargeWord.t) Iso.t
   val isoWord : (wordable, Word.t) Iso.t
   val isoWord8 : (wordable, Word8.t) Iso.t
   val toLargeWord : wordable -> LargeWord.t
   val toWord : wordable -> Word.t
   val toWord8 : wordable -> Word8.t
end

signature WORDABLE_X = sig
   include WORDABLE
   val fromWord8X : Word8.t -> wordable
   val fromWordX : Word.t -> wordable
   val isoLargeWordX : (wordable, LargeWord.t) Iso.t
   val isoWord8X : (wordable, Word8.t) Iso.t
   val isoWordX : (wordable, Word.t) Iso.t
   val toLargeWordX : wordable -> LargeWord.t
   val toWord8X : wordable -> Word8.t
   val toWordX : wordable -> Word.t
end
