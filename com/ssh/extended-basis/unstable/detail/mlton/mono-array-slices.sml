(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_ARRAY_SLICE} modules for MLton == *)

structure BoolArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisBoolArraySlice)

structure IntArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisIntArraySlice)
structure LargeIntArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisLargeIntArraySlice)
structure Int8ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisInt8ArraySlice)
structure Int16ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisInt16ArraySlice)
structure Int32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisInt32ArraySlice)
structure Int64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisInt64ArraySlice)

structure RealArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisRealArraySlice)
structure LargeRealArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisLargeRealArraySlice)
structure Real32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisReal32ArraySlice)
structure Real64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisReal64ArraySlice)

structure WordArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisWordArraySlice)
structure LargeWordArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisLargeWordArraySlice)
structure Word16ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisWord16ArraySlice)
structure Word32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisWord32ArraySlice)
structure Word64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisWord64ArraySlice)
