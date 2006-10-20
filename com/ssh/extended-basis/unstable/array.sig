(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {ARRAY} signature.
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
