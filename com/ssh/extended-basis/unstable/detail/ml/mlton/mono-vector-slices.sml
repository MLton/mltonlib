(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for MLton == *)

structure BoolVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisBoolVectorSlice)

structure IntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisIntVectorSlice)
structure LargeIntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisLargeIntVectorSlice)
structure Int8VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisInt8VectorSlice)
structure Int16VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisInt16VectorSlice)
structure Int32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisInt32VectorSlice)
structure Int64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisInt64VectorSlice)

structure RealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisRealVectorSlice)
structure LargeRealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisLargeRealVectorSlice)
structure Real32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisReal32VectorSlice)
structure Real64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisReal64VectorSlice)

structure WordVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisWordVectorSlice)
structure LargeWordVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisLargeWordVectorSlice)
structure Word16VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisWord16VectorSlice)
structure Word32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisWord32VectorSlice)
structure Word64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisWord64VectorSlice)
