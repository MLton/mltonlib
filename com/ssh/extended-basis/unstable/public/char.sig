(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {CHAR} signature. *)
signature CHAR = sig
   include CHAR

   type t = char
   (** Convenience alias. *)

   (** == Bounds == *)

   val minOrd : Int.t
   (** The least character code.  It always equals {0}. *)

   val boundsChar : t Sq.t
   (**
    * Pair of the least and greatest characters.  It always equals
    * {(minChar, maxChar)}.
    *)

   val boundsOrd : Int.t Sq.t
   (**
    * Pair of the least and greatest character codes.  It always equals
    * {(minOrd, maxOrd)}.
    *)

   (** == Isomorphisms == *)

   val isoInt : (t, Int.t) Iso.t
   (**
    * An isomorphism between characters and character codes.  It always
    * equals {(ord, chr)}.  Note that the projection part of the
    * isomorphism, namely {chr}, is likely to be a partial function.
    *)
end
