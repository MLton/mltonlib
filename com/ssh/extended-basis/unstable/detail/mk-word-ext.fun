(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {WORD} modules.
 *)
functor MkWordExt (W : WORD) = struct
   open W
   val bounds as (minWord, maxWord) = (fromInt 0, fromInt~1)
   val fromWord = fromLargeWord o Word.toLargeWord
   val fromWordX = fromLargeWord o Word.toLargeWordX
   val toWord = Word.fromLargeWord o toLargeWord
   val toWordX = Word.fromLargeWord o toLargeWordX
   val embString = (toString, fromString)
   val isoInt = (toInt, fromInt)
   val isoIntX = (toIntX, fromInt)
   val isoLarge = (toLargeWord, fromLargeWord)
   val isoLargeInt = (toLargeInt, fromLargeInt)
   val isoLargeIntX = (toLargeIntX, fromLargeInt)
   val isoLargeX = (toLargeWordX, fromLargeWord)
   val isoWord = (toWord, fromWord)
   val isoWordX = (toWordX, fromWordX)
   fun isZero w = fromInt 0 = w
   fun isEven w = isZero (andb (fromInt 1, w))
   val isOdd = not o isEven
end
