(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Vector :> VECTOR} structure.
 *)
structure Vector : VECTOR = struct
   local
      structure Common = MkSeqCommonExt (Vector)
   in
      open Vector Common
   end
end
