(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_ARRAY} modules for MLton.
 *)

structure BoolArray : MONO_ARRAY = MkMonoArrayExt (BoolArray)

structure IntArray : MONO_ARRAY = MkMonoArrayExt (IntArray)
structure LargeIntArray : MONO_ARRAY = MkMonoArrayExt (LargeIntArray)
structure Int8Array : MONO_ARRAY = MkMonoArrayExt (Int8Array)
structure Int16Array : MONO_ARRAY = MkMonoArrayExt (Int16Array)
structure Int32Array : MONO_ARRAY = MkMonoArrayExt (Int32Array)
structure Int64Array : MONO_ARRAY = MkMonoArrayExt (Int64Array)

structure RealArray : MONO_ARRAY = MkMonoArrayExt (RealArray)
structure LargeRealArray : MONO_ARRAY = MkMonoArrayExt (LargeRealArray)
structure Real32Array : MONO_ARRAY = MkMonoArrayExt (Real32Array)
structure Real64Array : MONO_ARRAY = MkMonoArrayExt (Real64Array)

structure WordArray : MONO_ARRAY = MkMonoArrayExt (WordArray)
structure LargeWordArray : MONO_ARRAY = MkMonoArrayExt (LargeWordArray)
structure Word8Array  : MONO_ARRAY = MkMonoArrayExt (Word8Array)
structure Word16Array : MONO_ARRAY = MkMonoArrayExt (Word16Array)
structure Word32Array : MONO_ARRAY = MkMonoArrayExt (Word32Array)
structure Word64Array : MONO_ARRAY = MkMonoArrayExt (Word64Array)
