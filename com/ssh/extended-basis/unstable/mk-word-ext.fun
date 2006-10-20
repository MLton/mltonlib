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
   val toWord = Word.fromLarge o toLarge
   val fromWord = fromLarge o Word.toLarge
   val intIso = (toInt, fromInt)
   val intXIso = (toIntX, fromInt)
   val largeIso = (toLarge, fromLarge)
   val largeIntIso = (toLargeInt, fromLargeInt)
   val largeIntXIso = (toLargeIntX, fromLargeInt)
   val largeXIso = (toLargeX, fromLarge)
   val wordIso = (toWord, fromWord)
   val stringEmb = (toString, fromString)
   fun isZero w = fromInt 0 = w
   fun isEven w = isZero (andb (fromInt 1, w))
   val isOdd = not o isEven
   val bounds as (minWord, maxWord) = (fromInt 0, fromInt~1)
end
