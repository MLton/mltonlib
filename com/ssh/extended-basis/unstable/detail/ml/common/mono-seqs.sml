(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Extended mono sequence modules common to all compilers *)

structure Word8Vector : MONO_VECTOR = MkMonoVectorExt (BasisWord8Vector)
structure Word8VectorSlice : MONO_VECTOR_SLICE =
   MkMonoVectorSliceExt (structure MonoVectorSlice = BasisWord8VectorSlice)
structure Word8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BasisWord8Array
                   structure MonoVector = BasisWord8Vector)
structure Word8ArraySlice : MONO_ARRAY_SLICE =
   MkMonoArraySliceExt (structure MonoArraySlice = BasisWord8ArraySlice)

structure Text : TEXT =
   MkTextExt (structure Text = BasisText
              open BasisByte
              val ch_0 = #"0" val ch_1 = #"1" val ch_7 = #"7" val ch_9 = #"9"
              val ch_a = #"a" val ch_f = #"f"
              val ch_A = #"A" val ch_F = #"F")
structure Char : CHAR = Text.Char
structure CharArray : MONO_ARRAY = Text.CharArray
structure CharArraySlice : MONO_ARRAY_SLICE = Text.CharArraySlice
structure CharVector : MONO_VECTOR = Text.CharVector
structure CharVectorSlice : MONO_VECTOR_SLICE = Text.CharVectorSlice
structure String : STRING = Text.String
structure Substring : SUBSTRING = Text.Substring
