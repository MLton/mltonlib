(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Vector :> VECTOR} structure.
 *)
structure Vector : VECTOR = struct
   open Vector
   type 'a t = 'a vector
   fun toList v = foldr op :: [] v
   val isoList = (toList, fromList)
end
