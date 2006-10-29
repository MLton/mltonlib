(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {LIST} signature.
 *)
signature LIST = sig
   include LIST

   val sub : 'a list * int -> 'a
   (**
    * {sub (l, i)} returns the {i}th element of the list {l}.  This is
    * equivalent to {nth}.
    *)
end
