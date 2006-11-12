(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {CHAR} signature.
 *)
signature CHAR = sig
   include CHAR

   type t = char
   (**
    * Convenience alias.
    *)

   (** == Bounds == *)

   val minOrd : Int.int
   (**
    * The least character code.  It always equals {0}.
    *)

   val boundsChar : char * char
   (**
    * Pair of the least and greatest characters.  It always equals
    * {(minChar, maxChar)}.
    *)

   val boundsOrd : Int.int * Int.int
   (**
    * Pair of the least and greatest character codes.  It always equals
    * {(minOrd, maxOrd)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : (char, Int.int) Iso.t
   (**
    * An isomorphism between characters and character codes.  It always
    * equals {(ord, chr)}.  Note that the projection part of the
    * isomorphism, namely {chr}, is likely to be a partial function.
    *)
end
