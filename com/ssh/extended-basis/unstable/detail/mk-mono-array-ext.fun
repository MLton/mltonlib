(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkMonoArrayExt (structure MonoVector : MONO_VECTOR
                        structure MonoArray : MONO_ARRAY
                           where type elem = MonoVector.elem
                           where type vector = MonoVector.vector) = struct
   local
      structure MonoArray = struct
         open MonoArray
         type t = array
      end
      structure Common = MkMonoSeqCommonExt (MonoArray)
   in
      open MonoArray Common
   end
   local
      fun mk tabulate length sub ? = tabulate (length ?, fn i => sub (?, i))
   in
      fun duplicate ? = mk tabulate length sub ?
      fun fromVector ? = mk tabulate MonoVector.length MonoVector.sub ?
      fun toPoly ? = mk Array.tabulate length sub ?
      fun fromPoly ? = mk tabulate Array.length Array.sub ?
   end
   val toVector = vector
   val isoVector = (toVector, fromVector)
   val isoPoly = (toPoly, fromPoly)
end
