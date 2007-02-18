(*
** 2007 February 18
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** $Id$
*)
signature PTR =
   sig
      type t
      val null: t
      val fromPtr: MLton.Pointer.t -> t
   end
structure Ptr =
   struct
      type t = MLton.Pointer.t
      val null = MLton.Pointer.null
      fun fromPtr x = x
   end
signature CSTR =
   sig
      type t
      type out
      val fromString: string -> t
      val toStringOpt: out -> string option
      val toString: out -> string
      val toStringLen: out * int -> string
   end
structure CStr =
   struct
      type t = string
      type out = MLton.Pointer.t
      
      val Pstrlen = _import "strlen": out -> int;
      
      fun fromString x = x
      
      (* You'd better be sure before you call this! *)
      fun cchr ptr i = (Byte.byteToChar o MLton.Pointer.getWord8) (ptr, i)
      fun toStringLen (ptr, len) = CharVector.tabulate (len, cchr ptr)
      fun toString ptr = toStringLen (ptr, Pstrlen ptr)
      
      fun toStringOpt ptr =
         if ptr = MLton.Pointer.null then NONE else SOME (toString ptr)
   end
signature BLOB =
   sig
      type t
      type out
      val fromVector: Word8Vector.vector -> t
      val toVector: out * int -> Word8Vector.vector
   end
structure Blob =
   struct
      type t = Word8Vector.vector
      type out = MLton.Pointer.t
      
      fun fromVector x = x
      
      fun toVector (ptr, len) =
         Word8Vector.tabulate (len, fn i => MLton.Pointer.getWord8 (ptr, i))
   end
