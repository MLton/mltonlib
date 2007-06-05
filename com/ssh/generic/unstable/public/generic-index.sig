(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the types of type-indices of generic functions.
 *)
signature GENERIC_INDEX = sig
   type 'a t
   (** Type of complete type-indices. *)

   type 'a s
   (** Type of incomplete sum type-indices. *)

   type ('a, 'k) p
   (** Type of incomplete product type-indices. *)
end
