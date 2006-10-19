(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {INTEGER} modules.
 *)

functor MkIntegerExt (I : INTEGER) =
   struct
      open I
      val int = (toInt, fromInt)
      val large = (toLarge, fromLarge)
      val string = (toString, fromString)
      fun isZero i = fromInt 0 = i
      fun isEven i = isZero (rem (i, fromInt 2))
      val isOdd = not o isEven
      val bounds = case (minInt, maxInt) of
                      (NONE, NONE) => NONE
                    | (SOME min, SOME max) => SOME (min, max)
                    | _ => raise Fail "impossible"
   end
