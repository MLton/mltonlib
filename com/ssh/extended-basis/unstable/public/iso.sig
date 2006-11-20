(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Signature for the {Iso} structure for isomorphisms. *)
signature ISO = sig
   type ('a, 'b) t = ('a -> 'b) * ('b -> 'a)
   (**
    * Isomorphism between {'a} and {'b} with injection and projection
    * functions.
    *)

   val id : ('a, 'a) t
   (**
    * The identity isomorphism.  This is always equivalent to {(fn a => a,
    * fn a => a)}.
    *)

   val to : ('a, 'b) t -> 'a -> 'b
   (** Extracts the injection part of the given isomorphism. *)

   val from : ('a, 'b) t -> 'b -> 'a
   (** Extracts the projection part of the given isomorphism. *)

   val swap : ('a, 'b) t -> ('b, 'a) t
   val map : ('c, 'a) t * ('b, 'd) t -> ('a, 'b) t -> ('c, 'd) t

   val <--> : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
   (** Isomorphism composition. *)

   val --> : ('c, 'a) t * ('b, 'd) t -> (('a, 'b) Fn.t, ('c, 'd) Fn.t) t
   val +` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Sum.t, ('c, 'd) Sum.t) t
   val *` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Product.t, ('c, 'd) Product.t) t
end
