(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {VECTOR_SLICE} signature. *)
signature VECTOR_SLICE = sig
   include BASIS_VECTOR_SLICE

   type 'a t = 'a slice
   (** Convenience alias. *)
end
