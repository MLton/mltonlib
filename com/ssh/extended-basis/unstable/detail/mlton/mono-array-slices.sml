(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY_SLICE} modules for MLton == *)

structure BoolArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BoolArraySlice)

structure IntArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = IntArraySlice)
structure LargeIntArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = LargeIntArraySlice)
structure Int8ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Int8ArraySlice)
structure Int16ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Int16ArraySlice)
structure Int32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Int32ArraySlice)
structure Int64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Int64ArraySlice)

structure RealArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = RealArraySlice)
structure LargeRealArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = LargeRealArraySlice)
structure Real32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Real32ArraySlice)
structure Real64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Real64ArraySlice)

structure WordArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = WordArraySlice)
structure LargeWordArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = LargeWordArraySlice)
structure Word16ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Word16ArraySlice)
structure Word32ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Word32ArraySlice)
structure Word64ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = Word64ArraySlice)
