(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Vector :> VECTOR} structure.
 *)

signature VECTOR =
   sig
      include VECTOR
      val listIso : ('a vector, 'a list) iso
      val toList : 'a vector -> 'a list
   end

structure Vector : VECTOR =
   struct
      open Vector
      fun toList v = foldr op :: [] v
      val listIso = (toList, fromList)
   end
