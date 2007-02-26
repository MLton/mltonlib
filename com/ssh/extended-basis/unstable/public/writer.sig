(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with writers. *)
signature WRITER = sig
   type ('a, 's) t = 'a * 's -> 's

   include CFUNC' where type ('a, 's) func = ('a, 's) t

   (** == Typing == *)

   type univ
   type 'a u = ('a, univ) t

   val polymorphically : ('a u -> 'b u) -> ('a, 's) t -> ('b, 's) t
end
