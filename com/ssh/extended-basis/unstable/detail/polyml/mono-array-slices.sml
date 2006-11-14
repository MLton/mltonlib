(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY_SLICE} modules for Poly/ML == *)

structure IntArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = IntArraySlice)

structure RealArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = RealArraySlice)
