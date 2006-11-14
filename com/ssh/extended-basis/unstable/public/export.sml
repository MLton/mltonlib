(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Implementation independent extensions == *)

signature ARRAY = ARRAY
signature ARRAY_SLICE = ARRAY_SLICE
signature BOOL = BOOL
signature CHAR = CHAR
signature EMB = EMB
signature INTEGER = INTEGER
signature INT_INF = INT_INF
signature ISO = ISO
signature LIST = LIST
signature MONO_ARRAY = MONO_ARRAY
signature MONO_ARRAY_SLICE = MONO_ARRAY_SLICE
signature MONO_VECTOR = MONO_VECTOR
signature MONO_VECTOR_SLICE = MONO_VECTOR_SLICE
signature OPTION = OPTION
signature REAL = REAL
signature STRING = STRING
signature SUBSTRING = SUBSTRING
signature TEXT = TEXT
signature VECTOR = VECTOR
signature VECTOR_SLICE = VECTOR_SLICE
signature WORD = WORD

structure Array : ARRAY = Array
structure ArraySlice : ARRAY_SLICE = ArraySlice
structure Bool : BOOL = Bool
structure Char : CHAR = Char
structure CharArray : MONO_ARRAY = CharArray
structure CharArraySlice : MONO_ARRAY_SLICE = CharArraySlice
structure CharVector : MONO_VECTOR = CharVector
structure CharVectorSlice : MONO_VECTOR_SLICE = CharVectorSlice
structure Emb : EMB = Emb
structure Int : INTEGER = Int
structure Iso : ISO = Iso
structure LargeInt : INTEGER = LargeInt
structure LargeReal : REAL = LargeReal
structure LargeWord : WORD = LargeWord
structure List : LIST = List
structure Option : OPTION = Option
structure Position : INTEGER = Position
structure Real : REAL = Real
structure String : STRING = String
structure Substring : SUBSTRING = Substring
structure Text : TEXT = Text
structure Vector : VECTOR = Vector
structure VectorSlice : VECTOR_SLICE = VectorSlice
structure Word : WORD = Word
structure Word8 : WORD = Word8
structure Word8Array : MONO_ARRAY = Word8Array
structure Word8ArraySlice : MONO_ARRAY_SLICE = Word8ArraySlice
structure Word8Vector : MONO_VECTOR = Word8Vector
structure Word8VectorSlice : MONO_VECTOR_SLICE = Word8VectorSlice
