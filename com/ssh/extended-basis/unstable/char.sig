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

      val intIso : (char, Int.int) iso
      (**
       * The isomorphism between characters and character codes.  It
       * always equals {(ord, chr)}.  Note that the projection part of the
       * isomorphism, namely {chr}, is likely to be a partial function.
       *)

      val boundsChar : char * char
      (**
       * Pair of the least and greatest characters.  It always equals
       * {(minChar, maxChar)}.
       *)

      val minOrd : Int.int
      (**
       * The least character code.  It always equals {0}.
       *)

      val boundsOrd : Int.int * Int.int
      (**
       * Pair of the least and greatest character codes.  It always equals
       * {(minOrd, maxOrd)}.
       *)
   end
