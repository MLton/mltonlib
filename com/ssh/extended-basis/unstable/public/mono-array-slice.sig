(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_ARRAY_SLICE} signature.
 *)
signature MONO_ARRAY_SLICE = sig
   include MONO_ARRAY_SLICE

   type t = slice
   (**
    * Convenience alias.
    *)
end
