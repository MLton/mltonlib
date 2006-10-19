(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {WORD} modules.
 *)

functor MkWordExt (W : WORD) =
   struct
      open W
      val toWord = Word.fromLarge o toLarge
      val fromWord = fromLarge o Word.toLarge
      val int = (toInt, fromInt)
      val intX = (toIntX, fromInt)
      val large = (toLarge, fromLarge)
      val largeInt = (toLargeInt, fromLargeInt)
      val largeIntX = (toLargeIntX, fromLargeInt)
      val largeX = (toLargeX, fromLarge)
      val word = (toWord, fromWord)
      val string = (toString, fromString)
      fun isZero w = fromInt 0 = w
      fun isEven w = isZero (andb (fromInt 1, w))
      val isOdd = not o isEven
      val bounds as (minWord, maxWord) = (fromInt 0, fromInt~1)
   end
