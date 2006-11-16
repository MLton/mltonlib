(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {Array :> ARRAY} structure.
 *)
structure Array : ARRAY = struct
   local
      structure Array = struct
         open Array
         type 'a t = 'a array
      end
      structure Common = MkSeqCommonExt (Array)
   in
      open Array Common
   end
   fun dup a = tabulate (length a, fn i => sub (a, i))
   val toVector = vector
   fun fromVector v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
   val isoVector = (toVector, fromVector)
end
