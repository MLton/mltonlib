(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY} modules for MLton == *)

structure BoolArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BoolArray
                   structure MonoVector = BoolVector)

structure IntArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = IntArray
                   structure MonoVector = IntVector)
structure LargeIntArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = LargeIntArray
                   structure MonoVector = LargeIntVector)
structure Int8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Int8Array
                   structure MonoVector = Int8Vector)
structure Int16Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Int16Array
                   structure MonoVector = Int16Vector)
structure Int32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Int32Array
                   structure MonoVector = Int32Vector)
structure Int64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Int64Array
                   structure MonoVector = Int64Vector)

structure RealArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = RealArray
                   structure MonoVector = RealVector)
structure LargeRealArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = LargeRealArray
                   structure MonoVector = LargeRealVector)
structure Real32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Real32Array
                   structure MonoVector = Real32Vector)
structure Real64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Real64Array
                   structure MonoVector = Real64Vector)

structure WordArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = WordArray
                   structure MonoVector = WordVector)
structure LargeWordArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = LargeWordArray
                   structure MonoVector = LargeWordVector)
structure Word8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word8Array
                   structure MonoVector = Word8Vector)
structure Word16Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word16Array
                   structure MonoVector = Word16Vector)
structure Word32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word32Array
                   structure MonoVector = Word32Vector)
structure Word64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word64Array
                   structure MonoVector = Word64Vector)
