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
      val list : ('a array, 'a list) iso
      val toList : 'a array -> 'a list
   end

structure Array : ARRAY =
   struct
      open Array
      fun toList v = foldr op :: [] v
      val list = (toList, fromList)
   end
