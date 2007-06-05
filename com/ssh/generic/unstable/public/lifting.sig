(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for combinators for lifting functions on values to functions
 * on the leaf elements of nested structures of pairs.  The user specifies
 * the path to a leaf element of a nested structure of pairs to get a
 * lifting index.
 *)
signature LIFTING = sig
   type ('element, 'of) t
   (** The type of lifting indices. *)

   (** == Lifting Operations == *)

   val get : ('a, 'b) t Thunk.t -> ('a -> 'c) -> 'b -> 'c
   (** Lift a get operation. *)

   val update : ('a, 'b) t Thunk.t -> 'a UnOp.t -> 'b UnOp.t
   (** Lift an update operation. *)

   (** == Creating Liftings == *)

   val id : ('a, 'a) t
   (** The identity lifting. *)

   val F : ('a, 'a * 'b) t
   (** Choose the first element of a pair. *)

   val S : ('b, 'a * 'b) t
   (** Choose the second element of a pair. *)

   val ^ : ('m, 'u) t * ('t, 'm) t -> ('t, 'u) t
   (** Concatenation of paths. *)
end
