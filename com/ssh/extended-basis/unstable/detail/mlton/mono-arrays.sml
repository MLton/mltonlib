(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_ARRAY} modules for MLton == *)

structure BoolArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisBoolArray
                   structure MonoVector = BasisBoolVector)

structure IntArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisIntArray
                   structure MonoVector = BasisIntVector)
structure LargeIntArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisLargeIntArray
                   structure MonoVector = BasisLargeIntVector)
structure Int8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisInt8Array
                   structure MonoVector = BasisInt8Vector)
structure Int16Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisInt16Array
                   structure MonoVector = BasisInt16Vector)
structure Int32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisInt32Array
                   structure MonoVector = BasisInt32Vector)
structure Int64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisInt64Array
                   structure MonoVector = BasisInt64Vector)

structure RealArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisRealArray
                   structure MonoVector = BasisRealVector)
structure LargeRealArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisLargeRealArray
                   structure MonoVector = BasisLargeRealVector)
structure Real32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisReal32Array
                   structure MonoVector = BasisReal32Vector)
structure Real64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisReal64Array
                   structure MonoVector = BasisReal64Vector)

structure WordArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisWordArray
                   structure MonoVector = BasisWordVector)
structure LargeWordArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisLargeWordArray
                   structure MonoVector = BasisLargeWordVector)
structure Word16Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisWord16Array
                   structure MonoVector = BasisWord16Vector)
structure Word32Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisWord32Array
                   structure MonoVector = BasisWord32Vector)
structure Word64Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisWord64Array
                   structure MonoVector = BasisWord64Vector)
