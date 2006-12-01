(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR_SLICE} modules for MLton == *)

structure BoolVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BoolVectorSlice)

structure IntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = IntVectorSlice)
structure LargeIntVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = LargeIntVectorSlice)
structure Int8VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Int8VectorSlice)
structure Int16VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Int16VectorSlice)
structure Int32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Int32VectorSlice)
structure Int64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Int64VectorSlice)

structure RealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = RealVectorSlice)
structure LargeRealVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = LargeRealVectorSlice)
structure Real32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Real32VectorSlice)
structure Real64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Real64VectorSlice)

structure WordVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = WordVectorSlice)
structure LargeWordVectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = LargeWordVectorSlice)
structure Word16VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Word16VectorSlice)
structure Word32VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Word32VectorSlice)
structure Word64VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = Word64VectorSlice)
