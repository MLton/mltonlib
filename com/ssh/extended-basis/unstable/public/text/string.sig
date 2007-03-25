(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {STRING} signature. *)
signature STRING = sig
   eqtype t
   (** Convenience alias. *)

   eqtype string
   eqtype char

   val maxSize : Int.t
   val size : t -> Int.t

   val sub : t * Int.t -> char

   val extract : t * Int.t * Int.t Option.t -> t
   val substring : t * Int.t * Int.t -> t

   val ^ : t BinOp.t

   val concat : t List.t -> t
   val concatWith : t -> t List.t -> t

   val str : char -> t

   val implode : char List.t -> t
   val explode : t -> char List.t

   val map : char UnOp.t -> t UnOp.t
   val translate : (char -> t) -> t UnOp.t

   val tokens : char UnPr.t -> t -> t List.t
   val fields : char UnPr.t -> t -> t List.t

   val isPrefix : t -> t UnPr.t
   val isSubstring : t -> t UnPr.t
   val isSuffix : t -> t UnPr.t

   val collate : char Cmp.t -> t Cmp.t

   (** == Bytes == *)

   val toBytes : t -> Word8Vector.t
   (** Converts given string to a byte vector. *)

   val fromBytes : Word8Vector.t -> t
   (** Converts given byte vector to a string. *)

   (** == Isomorphisms == *)

   val isoBytes : (t, Word8Vector.t) Iso.t
   (**
    * An isomorphism between strings and byte vectors.  It is always
    * equivalent to {(toBytes, fromBytes)}.
    *)

   val isoList : (t, char List.t) Iso.t
   (**
    * An isomorphism between strings and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   (** == {MONO_VECTOR} == *)

   type elem
   type vector

   val all : elem UnPr.t -> vector UnPr.t
   val app : elem Effect.t -> vector Effect.t
   val appi : (Int.t * elem) Effect.t -> vector Effect.t
   val exists : elem UnPr.t -> vector UnPr.t
   val find : elem UnPr.t -> vector -> elem Option.t
   val findi : (Int.t * elem -> Bool.t) -> vector -> (Int.t * elem) Option.t
   val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldli : (Int.t * elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldri : (Int.t * elem * 'a -> 'a) -> 'a -> vector -> 'a
   val fromList : elem List.t -> vector
   val fromPoly : elem Vector.t -> vector
   val isoPoly : (vector, elem Vector.t) Iso.t
   val length : vector -> Int.t
   val mapi : (Int.t * elem -> elem) -> vector UnOp.t
   val maxLen : Int.t
   val tabulate : Int.t * (Int.t -> elem) -> vector
   val toList : vector -> elem List.t
   val toPoly : vector -> elem Vector.t
   val update : vector * Int.t * elem -> vector

   (** == Concepts == *)

   include CASED CSTRINGABLE ORDERED SCANNABLE STRINGABLE

   sharing type char=elem
   sharing type t=string=cstringable=ordered=scannable=stringable=vector
end
