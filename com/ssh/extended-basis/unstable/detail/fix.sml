(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Fix :> FIX = struct
   type 'a t = 'a UnOp.t -> 'a
   exception Fix
end
