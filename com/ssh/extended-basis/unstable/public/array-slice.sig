(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {ARRAY_SLICE} signature.
 *)
signature ARRAY_SLICE = sig
   include ARRAY_SLICE

   type 'a t = 'a slice
   (**
    * Convenience alias.
    *)
end
