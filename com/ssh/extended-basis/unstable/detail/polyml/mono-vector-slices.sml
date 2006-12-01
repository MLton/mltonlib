(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for Poly/ML == *)

structure IntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = IntVectorSlice)

structure RealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = RealVectorSlice)
