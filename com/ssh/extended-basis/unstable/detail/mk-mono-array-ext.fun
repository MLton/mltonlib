(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {MONO_ARRAY} modules.
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
   fun dup a = tabulate (length a, fn i => sub (a, i))
   val toVector = vector
   fun fromVector v =
       tabulate (MonoVector.length v, fn i => MonoVector.sub (v, i))
   val isoVector = (toVector, fromVector)
   fun toPoly a = Array.tabulate (length a, fn i => sub (a, i))
   fun fromPoly a = tabulate (Array.length a, fn i => Array.sub (a, i))
   val isoPoly = (toPoly, fromPoly)
end
