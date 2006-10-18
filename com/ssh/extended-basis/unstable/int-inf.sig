(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {INT_INF} signature.
 *)

signature INT_INF =
   sig
      include INT_INF
      val int : (int, Int.int) iso
      val large : (int, LargeInt.int) iso
      val string : (int, string) emb
      val is0 : int -> bool
      val isEven : int -> bool
      val isOdd : int -> bool
      val bounds : (int * int) option
   end
