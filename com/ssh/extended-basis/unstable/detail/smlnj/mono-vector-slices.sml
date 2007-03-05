(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for SML/NJ == *)

structure RealVectorSlice =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisRealVectorSlice)
structure Real64VectorSlice =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisReal64VectorSlice)
