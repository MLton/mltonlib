(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Functor for extending {MONO_ARRAY_SLICE} modules.
 *)
functor MkMonoArraySliceExt (structure MonoArraySlice : MONO_ARRAY_SLICE) =
struct
   open MonoArraySlice
   type t = slice
end
