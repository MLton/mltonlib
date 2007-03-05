(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonoArrayExt (structure MonoVector : BASIS_MONO_VECTOR
                        structure MonoArray : BASIS_MONO_ARRAY
                           where type elem = MonoVector.elem
                           where type vector = MonoVector.vector) :
   MONO_ARRAY = struct
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
   fun map f a = tabulate (length a, fn i => f (sub (a, i)))
end
