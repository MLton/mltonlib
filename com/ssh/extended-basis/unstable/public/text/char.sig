(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {CHAR} signature. *)
signature CHAR = sig
   eqtype t
   (** Convenience alias. *)

   eqtype char
   eqtype string

   val ord : t -> Int.t
   val chr : Int.t -> t

   val succ : t UnOp.t
   val pred : t UnOp.t

   val contains : string -> t UnPr.t
   val notContains : string -> t UnPr.t

   (** == Digit Conversions ==
    *
    * Each of these raises {Domain} if the digit or int is not in the
    * correct range.
    *)

   val binDigitToInt : t -> Int.t
   val intToBinDigit : Int.t -> t
   val binDigitIsoInt : (t, Int.t) Iso.t

   val octDigitToInt : t -> Int.t
   val intToOctDigit : Int.t -> t
   val digitIsoInt : (t, Int.t) Iso.t

   val digitToInt : t -> Int.t
   val intToDigit : Int.t -> t
   val octDigitIsoInt : (t, Int.t) Iso.t

   val intToHexDigit : Int.t -> t
   val hexDigitToInt : t -> Int.t
   val hexDigitIsoInt : (t, Int.t) Iso.t

   (** == Character Predicates == *)

   val isBinDigit : t UnPr.t
   val isOctDigit : t UnPr.t
   val isDigit : t UnPr.t
   val isHexDigit : t UnPr.t

   val isAlpha : t UnPr.t
   val isAlphaNum : t UnPr.t
   val isAscii : t UnPr.t
   val isCntrl : t UnPr.t
   val isGraph : t UnPr.t
   val isLower : t UnPr.t
   val isPrint : t UnPr.t
   val isPunct : t UnPr.t
   val isSpace : t UnPr.t
   val isUpper : t UnPr.t

   (** == Bounds == *)

   val minChar : t
   val maxChar : t

   val boundsChar : t Sq.t
   (**
    * Pair of the least and greatest characters.  It always equals
    * {(minChar, maxChar)}.
    *)

   val maxOrd : Int.t
   val minOrd : Int.t
   (** The least character code.  It always equals {0}. *)

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

   (** == Concepts == *)

   include BOUNDED CASED CSTRINGABLE ORDERED SCANNABLE STRINGABLE

   sharing type t=char=bounded=cased=cstringable=ordered=scannable=stringable
end
