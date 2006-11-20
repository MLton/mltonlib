(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Sq :> SQ = struct
   open Sq
   fun mk x = (x, x)
   fun map f (x, y) = (f x, f y)
end
