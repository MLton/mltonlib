(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for SML/NJ == *)

structure RealVectorSlice =
   MkMonoVectorSliceExt (structure MonoVectorSlice = RealVectorSlice)
structure Real64VectorSlice =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Real64VectorSlice)
