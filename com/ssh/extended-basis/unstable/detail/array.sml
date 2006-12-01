(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Array : ARRAY = struct
   local
      structure Common = MkSeqCommonExt (Array)
   in
      open Array Common
   end
   fun duplicate a = tabulate (length a, fn i => sub (a, i))
   val toVector = vector
   fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
   val isoVector = (toVector, fromVector)
end
