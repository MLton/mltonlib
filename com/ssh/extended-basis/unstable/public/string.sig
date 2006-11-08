(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {STRING} signature.
 *)
signature STRING = sig
   include STRING

   type t = string
   (**
    * Convenience alias.
    *)

   (** == Embeddings == *)

   val embCString : (string, string) Emb.emb
   (**
    * An embedding of strings into C-style string literals.  It is always
    * equivalent to {(toCString, fromCString)}.
    *)

   val embString : (string, string) Emb.emb
   (**
    * An embedding of strings into SML-style string literals.  It is
    * always equivalent to {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoList : (string, char list) Iso.iso
   (**
    * An isomorphism between strings and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   (** == {MONO_VECTOR} == *)

   type elem = char
   type vector = string

   val all : (elem -> bool) -> vector -> bool
   val app  : (elem -> unit) -> vector -> unit
   val appi : (int * elem -> unit) -> vector -> unit
   val exists : (elem -> bool) -> vector -> bool
   val find  : (elem -> bool) -> vector -> elem option
   val findi : (int * elem -> bool) -> vector -> (int * elem) option
   val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
   val fromList : elem list -> vector
   val fromPoly : elem Vector.vector -> vector
   val isoPoly : (vector, elem Vector.vector) Iso.iso
   val length : vector -> int
   val mapi : (int * elem -> elem) -> vector -> vector
   val maxLen : int
   val tabulate : int * (int -> elem) -> vector
   val toList : vector -> elem list
   val toPoly : vector -> elem Vector.vector
   val update : vector * int * elem -> vector
end
