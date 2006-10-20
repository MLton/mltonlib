(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {INTEGER} signature.
 *)
signature INTEGER = sig
   include INTEGER

   (** == Bounds == *)

   val bounds : (int * int) option

   (** == Embeddings == *)

   val stringEmb : (int, string) emb

   (** == Isomorphisms == *)

   val intIso : (int, Int.int) iso
   val largeIso : (int, LargeInt.int) iso

   (** == Predicates == *)

   val isEven : int -> bool
   val isOdd : int -> bool
   val isZero : int -> bool
end
