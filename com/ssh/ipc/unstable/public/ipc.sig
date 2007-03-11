(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Inter Process Communication library.
 *)
signature IPC = sig
   (**
    * Type indices for IPC.  Only bounded-size data is allowed for
    * efficiency and simplicity.
    *)
   structure Type : sig
      type 'a t

      (** == User Defined Types == *)

      val iso : 'b t -> ('a, 'b) Iso.t -> 'a t

      (** == Products == *)

      type 'a p
      val tuple : 'a p -> 'a t
      val T : 'a t -> 'a p
      val *` : 'a p * 'b p -> ('a, 'b) Product.t p

      (** == Sums == *)

      type 'a s
      val data : 'a s -> 'a t
      val C0 : Unit.t s
      val C1 : 'a t -> 'a s
      val +` : 'a s * 'b s -> ('a, 'b) Sum.t s

      (** == Primitive Types == *)

      val unit : Unit.t t

      val int8  : Int8.t  t
      val int16 : Int16.t t
      val int32 : Int32.t t
      val int64 : Int64.t t

      val word8  : Word8.t  t
      val word16 : Word16.t t
      val word32 : Word32.t t
      val word64 : Word64.t t

      val real32 : Real32.t t
      val real64 : Real64.t t
   end
end
