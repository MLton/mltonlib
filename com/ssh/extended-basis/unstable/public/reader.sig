(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with readers. *)
signature READER = sig
   type ('a, 'b) t = 'b -> ('a * 'b) Option.t

   (** == Monad Interface == *)

   val return : 'a -> ('a, 's) t
   val >>= : ('a, 's) t * ('a -> ('b, 's) t) -> ('b, 's) t

   (** == Useful Combinators == *)

   val map : ('a -> 'b) -> ('a, 's) t -> ('b, 's) t
   val >>& : ('a, 's) t * ('b, 's) t -> (('a, 'b) Product.t, 's) t

   (** == Typing == *)

   type univ
   type 'a u = ('a, univ) t

   val polymorphically : ('a u -> 'b u) -> ('a, 's) t -> ('b, 's) t
end