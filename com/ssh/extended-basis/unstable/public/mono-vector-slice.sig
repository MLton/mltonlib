(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_VECTOR_SLICE} signature.
 *)
signature MONO_VECTOR_SLICE = sig
   include MONO_VECTOR_SLICE

   type t = slice
   (**
    * Convenience alias.
    *)
end
