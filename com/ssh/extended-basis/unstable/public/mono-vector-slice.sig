(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {MONO_VECTOR_SLICE} signature. *)
signature MONO_VECTOR_SLICE = sig
   include MONO_VECTOR_SLICE

   type t = slice
   (** Convenience alias. *)
end
