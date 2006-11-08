(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {BOOL} signature.
 *)
signature BOOL = sig
   include BOOL

   type t = bool
   (**
    * Convenience alias.
    *)
end
