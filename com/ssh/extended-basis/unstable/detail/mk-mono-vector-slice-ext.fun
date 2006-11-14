(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {MONO_VECTOR_SLICE} modules.
 *)
functor MkMonoVectorSliceExt (structure MonoVectorSlice : MONO_VECTOR_SLICE) =
struct
   open MonoVectorSlice
   type t = slice
end
