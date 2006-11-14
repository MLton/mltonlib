(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for Poly/ML == *)

structure IntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = IntVectorSlice)

structure RealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = RealVectorSlice)
