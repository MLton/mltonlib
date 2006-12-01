(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Vector : VECTOR = struct
   local
      structure Common = MkSeqCommonExt (Vector)
   in
      open Vector Common
   end
end
