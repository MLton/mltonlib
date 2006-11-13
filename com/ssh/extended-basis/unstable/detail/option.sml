(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Option : OPTION} structure.
 *)
structure Option : OPTION = struct
   open Option
   type 'a t = 'a option
   val isNone = fn NONE   => true
                 | SOME _ => false
end
