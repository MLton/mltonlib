(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkMonoVectorSliceExt (structure MonoVectorSlice :
                                 BASIS_MONO_VECTOR_SLICE) =
struct
   open MonoVectorSlice
   type t = slice
end
