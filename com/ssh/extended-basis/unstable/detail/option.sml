(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Option : OPTION = struct
   open Option
   val isNone = fn NONE   => true
                 | SOME _ => false
end
