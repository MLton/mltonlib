(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Array :> ARRAY} structure.
 *)
structure Array : ARRAY = struct
   open Array
   type 'a t = 'a array
   fun toList a = foldr op :: [] a
   val isoList = (toList, fromList)
   val toVector = vector
   fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
   val isoVector = (toVector, fromVector)
end
