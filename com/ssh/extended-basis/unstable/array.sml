(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Array :> ARRAY} structure.
 *)

structure Array : ARRAY =
   struct
      open Array
      fun toList v = foldr op :: [] v
      val listIso = (toList, fromList)
      val toVector = vector
      fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
      val vectorIso = (toVector, fromVector)
   end
