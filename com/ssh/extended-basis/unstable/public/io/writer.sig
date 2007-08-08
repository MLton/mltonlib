(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with writers. *)
signature WRITER = sig
   type ('a, 's) t = 'a * 's -> 's

   val mapState : ('s, 't) Iso.t -> ('a, 't) t -> ('a, 's) t

   (** == Functor Interface == *)

   type 'a func_dom and 'a func_cod
   include CFUNC where type 'a func = 'a func_dom -> 'a func_cod

   val polymorphically : ('a func -> 'b func) -> ('a, 's) t -> ('b, 's) t
end
