(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {INTEGER} signature.
 *)

signature INTEGER =
   sig
      include INTEGER
      val int : (int, Int.int) iso
      val large : (int, LargeInt.int) iso
      val string : (int, string) emb
      val is0 : int -> bool
      val isEven : int -> bool
      val isOdd : int -> bool
      val bounds : (int * int) option
   end
