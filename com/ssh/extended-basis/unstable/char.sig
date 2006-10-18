(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {CHAR} signature.
 *)

signature CHAR =
   sig
      include CHAR
      val int : (char, Int.int) iso
      val minOrd : Int.int
      val boundsChar : char * char
      val boundsOrd : Int.int * Int.int
   end
