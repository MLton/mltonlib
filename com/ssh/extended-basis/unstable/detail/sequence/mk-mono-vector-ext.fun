(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonoVectorExt (MonoVector : BASIS_MONO_VECTOR) : MONO_VECTOR = struct
   local
      structure MonoVector = struct
         open MonoVector
         type t = vector
      end
      structure Common = MkMonoSeqCommonExt (MonoVector)
   in
      open MonoVector Common
   end
   (* XXX It would be nice to avoid copying in toPoly and fromPoly *)
   fun toPoly v = Vector.tabulate (length v, fn i => sub (v, i))
   fun fromPoly v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
   val isoPoly = (toPoly, fromPoly)
end
