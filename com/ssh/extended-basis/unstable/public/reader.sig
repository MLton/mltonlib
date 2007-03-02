(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Utilities for dealing with readers. *)
signature READER = sig
   type ('a, 's) t = 's -> ('a * 's) Option.t

   (** == Monad Interface == *)

   type 'a monad_d and 'a monad_r
   include MONADP_CORE where type 'a monad = 'a monad_d -> 'a monad_r
   structure Monad : MONADP where type 'a monad = 'a monad

   val polymorphically : ('a monad -> 'b monad) -> ('a, 's) t -> ('b, 's) t
end
