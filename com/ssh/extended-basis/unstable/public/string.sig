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

   val embCString : (t, t) Emb.t
   (**
    * An embedding of strings into C-style string literals.  It is always
    * equivalent to {(toCString, fromCString)}.
    *)

   val embString : (t, t) Emb.t
   (**
    * An embedding of strings into SML-style string literals.  It is
    * always equivalent to {(toString, fromString)}.
    *)

   (** == Isomorphisms == *)

   val isoList : (t, char List.t) Iso.t
   (**
    * An isomorphism between strings and lists.  It is always equivalent
    * to {(toList, fromList)}.
    *)

   (** == {MONO_VECTOR} == *)

   type elem = char
   type vector = t

   val all : elem UnPr.t -> vector UnPr.t
   val app  : elem Effect.t -> vector Effect.t
   val appi : (Int.t * elem) Effect.t -> vector Effect.t
   val exists : elem UnPr.t -> vector UnPr.t
   val find  : elem UnPr.t -> vector -> elem Option.t
   val findi : (Int.t * elem -> Bool.t) -> vector -> (Int.t * elem) Option.t
   val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldli : (Int.t * elem * 'a -> 'a) -> 'a -> vector -> 'a
   val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
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
end
