(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Array :> ARRAY} structure.
 *)

signature ARRAY =
   sig
      include ARRAY
      val toList : 'a array -> 'a list
      val listIso : ('a array, 'a list) iso
      val toVector : 'a array -> 'a vector
      val fromVector : 'a vector -> 'a array
      val vectorIso : ('a array, 'a vector) iso
   end

structure Array : ARRAY =
   struct
      open Array
      fun toList v = foldr op :: [] v
      val listIso = (toList, fromList)
      val toVector = vector
      fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
      val vectorIso = (toVector, fromVector)
   end
