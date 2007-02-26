(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with readers. *)
signature READER = sig
   type ('a, 's) t = 's -> ('a * 's) Option.t

   include MONAD' where type ('a, 's) monad = ('a, 's) t

   (** == Typing == *)

   type univ
   type 'a u = ('a, univ) t

   val polymorphically : ('a u -> 'b u) -> ('a, 's) t -> ('b, 's) t
end
