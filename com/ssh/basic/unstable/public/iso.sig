(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with isomorphisms. *)
signature ISO = sig
   include ISO

   val swap : ('a, 'b) t -> ('b, 'a) t

   val map : ('c, 'a) t * ('b, 'd) t -> ('a, 'b) t -> ('c, 'd) t

   val <--> : ('a, 'b) t * ('c, 'a) t -> ('c, 'b) t
   (**
    * Isomorphism composition.
    *)

   val --> : ('c, 'a) t * ('b, 'd) t -> (('a, 'b) Fn.t, ('c, 'd) Fn.t) t
   val +` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Sum.t, ('c, 'd) Sum.t) t
   val *` : ('a, 'c) t * ('b, 'd) t -> (('a, 'b) Product.t, ('c, 'd) Product.t) t
end
