(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonoArraySliceExt (structure MonoArraySlice :
                                BASIS_MONO_ARRAY_SLICE) : MONO_ARRAY_SLICE =
struct
   open MonoArraySlice
   type t = slice
end
