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
      val intIso : (int, Int.int) iso
      val largeIso : (int, LargeInt.int) iso
      val stringEmb : (int, string) emb
      val isZero : int -> bool
      val isEven : int -> bool
      val isOdd : int -> bool
      val bounds : (int * int) option
   end
