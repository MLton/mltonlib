(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Signature for the {Iso} structure for isomorphisms. *)
signature ISO = sig
   type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
   (** Witness to an isomorphism between {'a} and {'b}. *)

   val id : ('a, 'a) t
   (** The trivial isomorphism.  This is always equivalent to {(id, id)}. *)

   (** == Basic == *)

   val to : ('a, 'b) t -> 'a -> 'b
   (** Extracts the injection part of the given isomorphism. *)

   val from : ('a, 'b) t -> 'b -> 'a
   (** Extracts the projection part of the given isomorphism. *)

   val swap : ('a, 'b) t -> ('b, 'a) t
   (** Switch the direction of the isomorphism. *)

   (** == Combinators for Building Isomorphisms == *)

   val map : ('c, 'a) t * ('b, 'd) t -> ('a, 'b) t -> ('c, 'd) t
   (** Changes the domain and range of an isomorphism. *)

   val <--> : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
   (** Isomorphism composition. *)

   val --> : ('c, 'a) t * ('b, 'd) t -> (('a, 'b) Fn.t, ('c, 'd) Fn.t) t
   (**
    * Creates an isomorphism between functions given isomorphisms between
    * domains and ranges.
    *)

   val +` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Sum.t, ('c, 'd) Sum.t) t
   (**
    * Creates an isomorphism between sums given isomorphisms between
    * elements.
    *)

   val *` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Product.t, ('c, 'd) Product.t) t
   (**
    * Creates an isomorphism between products given isomorphisms between
    * elements.
    *)
end
