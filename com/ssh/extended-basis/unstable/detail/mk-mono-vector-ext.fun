(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {MONO_VECTOR} modules.
 *)
functor MkMonoVectorExt (M : MONO_VECTOR) = struct
   open M
   type t = vector
   fun toList v = foldr op :: [] v
   val isoList = (toList, fromList)
   (* XXX It would be nice to avoid copying in toPoly and fromPoly *)
   fun toPoly v = Vector.tabulate (length v, fn i => sub (v, i))
   fun fromPoly v = tabulate (Vector.length v, fn i => Vector.sub (v, i))
   val isoPoly = (toPoly, fromPoly)
end
