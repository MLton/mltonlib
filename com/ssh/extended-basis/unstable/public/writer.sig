(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Utilities for dealing with writers. *)
signature WRITER = sig
   type ('a, 'b) t = 'a * 'b -> 'b

   (** == Useful Combinators == *)

   val map : ('b -> 'a) -> ('a, 's) t -> ('b, 's) t

   (** == Typing == *)

   type univ
   type 'a u = ('a, univ) t

   val polymorphically : ('a u -> 'b u) -> ('a, 's) t -> ('b, 's) t
end
