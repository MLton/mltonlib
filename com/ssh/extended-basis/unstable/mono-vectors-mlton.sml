(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {MONO_VECTOR} modules for MLton.
 *)

structure BoolVector : MONO_VECTOR = MkMonoVectorExt (BoolVector)

structure IntVector : MONO_VECTOR = MkMonoVectorExt (IntVector)
structure LargeIntVector : MONO_VECTOR = MkMonoVectorExt (LargeIntVector)
structure Int8Vector : MONO_VECTOR = MkMonoVectorExt (Int8Vector)
structure Int16Vector : MONO_VECTOR = MkMonoVectorExt (Int16Vector)
structure Int32Vector : MONO_VECTOR = MkMonoVectorExt (Int32Vector)
structure Int64Vector : MONO_VECTOR = MkMonoVectorExt (Int64Vector)

structure RealVector : MONO_VECTOR = MkMonoVectorExt (RealVector)
structure LargeRealVector : MONO_VECTOR = MkMonoVectorExt (LargeRealVector)
structure Real32Vector : MONO_VECTOR = MkMonoVectorExt (Real32Vector)
structure Real64Vector : MONO_VECTOR = MkMonoVectorExt (Real64Vector)

structure WordVector : MONO_VECTOR = MkMonoVectorExt (WordVector)
structure LargeWordVector : MONO_VECTOR = MkMonoVectorExt (LargeWordVector)
structure Word8Vector  : MONO_VECTOR = MkMonoVectorExt (Word8Vector)
structure Word16Vector : MONO_VECTOR = MkMonoVectorExt (Word16Vector)
structure Word32Vector : MONO_VECTOR = MkMonoVectorExt (Word32Vector)
structure Word64Vector : MONO_VECTOR = MkMonoVectorExt (Word64Vector)
