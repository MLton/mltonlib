(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** A mutable integer object holds an integer value. *)
signature INT_OBJ = sig
   eqtype t                      (** The type of mutable integer objects. *)
   val new : Int.t -> t          (** Allocates a new object with given value. *)
   val discard : t Effect.t      (** Deallocates the object. *)
   val get : t -> Int.t          (** Returns the value of the object. *)
   val set : t -> Int.t Effect.t (** Sets the value of the object. *)
end
