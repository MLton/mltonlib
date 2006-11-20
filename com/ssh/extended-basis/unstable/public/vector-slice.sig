(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** Extended {VECTOR_SLICE} signature. *)
signature VECTOR_SLICE = sig
   include VECTOR_SLICE

   type 'a t = 'a slice
   (** Convenience alias. *)
end
