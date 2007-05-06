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
   val isoLargeWord : (wordable, LargeWord.t) Iso.t
   val isoWord : (wordable, Word.t) Iso.t
   val toLargeWord : wordable -> LargeWord.t
   val toWord : wordable -> Word.t
end
