(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR} modules for MLton == *)

structure BoolVector : MONO_VECTOR = MkMonoVectorExt (BasisBoolVector)

structure IntVector : MONO_VECTOR = MkMonoVectorExt (BasisIntVector)
structure LargeIntVector : MONO_VECTOR = MkMonoVectorExt (BasisLargeIntVector)
structure Int8Vector : MONO_VECTOR = MkMonoVectorExt (BasisInt8Vector)
structure Int16Vector : MONO_VECTOR = MkMonoVectorExt (BasisInt16Vector)
structure Int32Vector : MONO_VECTOR = MkMonoVectorExt (BasisInt32Vector)
structure Int64Vector : MONO_VECTOR = MkMonoVectorExt (BasisInt64Vector)

structure RealVector : MONO_VECTOR = MkMonoVectorExt (BasisRealVector)
structure LargeRealVector : MONO_VECTOR = MkMonoVectorExt (BasisLargeRealVector)
structure Real32Vector : MONO_VECTOR = MkMonoVectorExt (BasisReal32Vector)
structure Real64Vector : MONO_VECTOR = MkMonoVectorExt (BasisReal64Vector)

structure WordVector : MONO_VECTOR = MkMonoVectorExt (BasisWordVector)
structure LargeWordVector : MONO_VECTOR = MkMonoVectorExt (BasisLargeWordVector)
structure Word16Vector : MONO_VECTOR = MkMonoVectorExt (BasisWord16Vector)
structure Word32Vector : MONO_VECTOR = MkMonoVectorExt (BasisWord32Vector)
structure Word64Vector : MONO_VECTOR = MkMonoVectorExt (BasisWord64Vector)
