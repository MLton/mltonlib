(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == MLton specific extensions == *)

structure BoolArray : MONO_ARRAY = BoolArray
structure BoolArraySlice : MONO_ARRAY_SLICE = BoolArraySlice
structure BoolVector : MONO_VECTOR = BoolVector
structure BoolVectorSlice : MONO_VECTOR_SLICE = BoolVectorSlice
structure FixedInt : INTEGER = FixedInt
structure Int1 : INTEGER = Int1
structure Int10 : INTEGER = Int10
structure Int11 : INTEGER = Int11
structure Int12 : INTEGER = Int12
structure Int13 : INTEGER = Int13
structure Int14 : INTEGER = Int14
structure Int15 : INTEGER = Int15
structure Int16 : INTEGER = Int16
structure Int16Array : MONO_ARRAY = Int16Array
structure Int16ArraySlice : MONO_ARRAY_SLICE = Int16ArraySlice
structure Int16Vector : MONO_VECTOR = Int16Vector
structure Int16VectorSlice : MONO_VECTOR_SLICE = Int16VectorSlice
structure Int17 : INTEGER = Int17
structure Int18 : INTEGER = Int18
structure Int19 : INTEGER = Int19
structure Int2 : INTEGER = Int2
structure Int20 : INTEGER = Int20
structure Int21 : INTEGER = Int21
structure Int22 : INTEGER = Int22
structure Int23 : INTEGER = Int23
structure Int24 : INTEGER = Int24
structure Int25 : INTEGER = Int25
structure Int26 : INTEGER = Int26
structure Int27 : INTEGER = Int27
structure Int28 : INTEGER = Int28
structure Int29 : INTEGER = Int29
structure Int3 : INTEGER = Int3
structure Int30 : INTEGER = Int30
structure Int31 : INTEGER = Int31
structure Int32 : INTEGER = Int32
structure Int32Array : MONO_ARRAY = Int32Array
structure Int32ArraySlice : MONO_ARRAY_SLICE = Int32ArraySlice
structure Int32Vector : MONO_VECTOR = Int32Vector
structure Int32VectorSlice : MONO_VECTOR_SLICE = Int32VectorSlice
structure Int4 : INTEGER = Int4
structure Int5 : INTEGER = Int5
structure Int6 : INTEGER = Int6
structure Int64 : INTEGER = Int64
structure Int64Array : MONO_ARRAY = Int64Array
structure Int64ArraySlice : MONO_ARRAY_SLICE = Int64ArraySlice
structure Int64Vector : MONO_VECTOR = Int64Vector
structure Int64VectorSlice : MONO_VECTOR_SLICE = Int64VectorSlice
structure Int7 : INTEGER = Int7
structure Int8 : INTEGER = Int8
structure Int8Array : MONO_ARRAY = Int8Array
structure Int8ArraySlice : MONO_ARRAY_SLICE = Int8ArraySlice
structure Int8Vector : MONO_VECTOR = Int8Vector
structure Int8VectorSlice : MONO_VECTOR_SLICE = Int8VectorSlice
structure Int9 : INTEGER = Int9
structure IntArray : MONO_ARRAY = IntArray
structure IntArraySlice : MONO_ARRAY_SLICE = IntArraySlice
structure IntInf : INT_INF = IntInf
structure IntVector : MONO_VECTOR = IntVector
structure IntVectorSlice : MONO_VECTOR_SLICE = IntVectorSlice
structure LargeIntArray : MONO_ARRAY = LargeIntArray
structure LargeIntArraySlice : MONO_ARRAY_SLICE = LargeIntArraySlice
structure LargeIntVector : MONO_VECTOR = LargeIntVector
structure LargeIntVectorSlice : MONO_VECTOR_SLICE = LargeIntVectorSlice
structure LargeRealArray : MONO_ARRAY = LargeRealArray
structure LargeRealArraySlice : MONO_ARRAY_SLICE = LargeRealArraySlice
structure LargeRealVector : MONO_VECTOR = LargeRealVector
structure LargeRealVectorSlice : MONO_VECTOR_SLICE = LargeRealVectorSlice
structure LargeWordArray : MONO_ARRAY = LargeWordArray
structure LargeWordArraySlice : MONO_ARRAY_SLICE = LargeWordArraySlice
structure LargeWordVector : MONO_VECTOR = LargeWordVector
structure LargeWordVectorSlice : MONO_VECTOR_SLICE = LargeWordVectorSlice
structure Real32 : REAL = Real32
structure Real32Array : MONO_ARRAY = Real32Array
structure Real32ArraySlice : MONO_ARRAY_SLICE = Real32ArraySlice
structure Real32Vector : MONO_VECTOR = Real32Vector
structure Real32VectorSlice : MONO_VECTOR_SLICE = Real32VectorSlice
structure Real64 : REAL = Real64
structure Real64Array : MONO_ARRAY = Real64Array
structure Real64ArraySlice : MONO_ARRAY_SLICE = Real64ArraySlice
structure Real64Vector : MONO_VECTOR = Real64Vector
structure Real64VectorSlice : MONO_VECTOR_SLICE = Real64VectorSlice
structure RealArray : MONO_ARRAY = RealArray
structure RealArraySlice : MONO_ARRAY_SLICE = RealArraySlice
structure RealVector : MONO_VECTOR = RealVector
structure RealVectorSlice : MONO_VECTOR_SLICE = RealVectorSlice
structure SysWord : WORD = SysWord
structure SysWordFlags : FLAGS = MkWordFlags (SysWord)
structure Word1 : WORD = Word1
structure Word10 : WORD = Word10
structure Word11 : WORD = Word11
structure Word12 : WORD = Word12
structure Word13 : WORD = Word13
structure Word14 : WORD = Word14
structure Word15 : WORD = Word15
structure Word16 : WORD = Word16
structure Word16Array : MONO_ARRAY = Word16Array
structure Word16ArraySlice : MONO_ARRAY_SLICE = Word16ArraySlice
structure Word16Flags : FLAGS = MkWordFlags (Word16)
structure Word16Vector : MONO_VECTOR = Word16Vector
structure Word16VectorSlice : MONO_VECTOR_SLICE = Word16VectorSlice
structure Word17 : WORD = Word17
structure Word18 : WORD = Word18
structure Word19 : WORD = Word19
structure Word2 : WORD = Word2
structure Word20 : WORD = Word20
structure Word21 : WORD = Word21
structure Word22 : WORD = Word22
structure Word23 : WORD = Word23
structure Word24 : WORD = Word24
structure Word25 : WORD = Word25
structure Word26 : WORD = Word26
structure Word27 : WORD = Word27
structure Word28 : WORD = Word28
structure Word29 : WORD = Word29
structure Word3 : WORD = Word3
structure Word30 : WORD = Word30
structure Word31 : WORD = Word31
structure Word32 : WORD = Word32
structure Word32Array : MONO_ARRAY = Word32Array
structure Word32ArraySlice : MONO_ARRAY_SLICE = Word32ArraySlice
structure Word32Flags : FLAGS = MkWordFlags (Word32)
structure Word32Vector : MONO_VECTOR = Word32Vector
structure Word32VectorSlice : MONO_VECTOR_SLICE = Word32VectorSlice
structure Word4 : WORD = Word4
structure Word5 : WORD = Word5
structure Word6 : WORD = Word6
structure Word64 : WORD = Word64
structure Word64Array : MONO_ARRAY = Word64Array
structure Word64ArraySlice : MONO_ARRAY_SLICE = Word64ArraySlice
structure Word64Flags : FLAGS = MkWordFlags (Word64)
structure Word64Vector : MONO_VECTOR = Word64Vector
structure Word64VectorSlice : MONO_VECTOR_SLICE = Word64VectorSlice
structure Word7 : WORD = Word7
structure Word9 : WORD = Word9
structure WordArray : MONO_ARRAY = WordArray
structure WordArraySlice : MONO_ARRAY_SLICE = WordArraySlice
structure WordVector : MONO_VECTOR = WordVector
structure WordVectorSlice : MONO_VECTOR_SLICE = WordVectorSlice
