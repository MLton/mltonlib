(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature PRODUCT_TYPE = sig
   type ('a, 'b) t

   (** == Conversions == *)

   val fromTuple2 : 'a * 'b -> ('a, 'b) t
   val fromTuple3 : 'a * 'b * 'c -> (('a, 'b) t, 'c) t
   val fromTuple4 : 'a * 'b * 'c * 'd -> ((('a, 'b) t, 'c) t, 'd) t
   (**
    * Conversions from tuples to products.
    *)

   val toTuple2 : ('a, 'b) t -> 'a * 'b
   val toTuple3 : (('a, 'b) t, 'c) t -> 'a * 'b * 'c
   val toTuple4 : ((('a, 'b) t, 'c) t, 'd) t -> 'a * 'b * 'c * 'd
   (**
    * Conversions from products to tuples.
    *)

   (** == Isomorphisms == *)

   val isoTuple2 : ('a * 'b, ('a, 'b) t) Iso.t
   val isoTuple3 : ('a * 'b * 'c, (('a, 'b) t, 'c) t) Iso.t
   val isoTuple4 : ('a * 'b * 'c * 'd, ((('a, 'b) t, 'c) t, 'd) t) Iso.t
   (**
    * Isomorphisms between products and tuples.
    *)

   (** == Operations == *)

   val swap : ('a, 'b) t -> ('b, 'a) t
   (** Swap the elements of a product. *)

   val swizzle : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
   (** Swizzle the elements of a pair of products. *)

   val fst : ('a, 'b) t -> 'a
   (** Extract the first element of a product. *)

   val snd : ('a, 'b) t -> 'b
   (** Extract the second element of a product. *)

   (** == HOFs for Building Operations on Products == *)

   val app : 'a Effect.t * 'b Effect.t -> ('a, 'b) t Effect.t
   val appFst : 'a Effect.t -> ('a, 'b) t Effect.t
   val appSnd : 'b Effect.t -> ('a, 'b) t Effect.t

   val map : ('a -> 'c) * ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
   val mapFst : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
   val mapSnd : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

   val all : 'a UnPr.t * 'b UnPr.t -> ('a, 'b) t UnPr.t
   val exists : 'a UnPr.t * 'b UnPr.t -> ('a, 'b) t UnPr.t

   val equal : 'a BinPr.t * 'b BinPr.t -> ('a, 'b) t BinPr.t
   val collate : 'a Cmp.t * 'b Cmp.t -> ('a, 'b) t Cmp.t

   val foldl : ('a * 'x -> 'y) * ('b * 'y -> 'z) -> ('a, 'b) t * 'x -> 'z
   val foldr : ('a * 'y -> 'z) * ('b * 'x -> 'y) -> ('a, 'b) t * 'x -> 'z

   val thunk : 'a Thunk.t * 'b Thunk.t -> ('a, 'b) t Thunk.t

   (** == Generic Programming == *)

   val iso : ('a, 'c) Iso.t * ('b, 'd) Iso.t -> (('a, 'b) t, ('c, 'd) t) Iso.t
   (** Lifts isos between elements to an iso between products. *)
end
