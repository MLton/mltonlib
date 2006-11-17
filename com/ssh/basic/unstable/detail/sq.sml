(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Sq :> SQ = struct
   type 'a t = 'a * 'a
   fun mk x = (x, x)
end
