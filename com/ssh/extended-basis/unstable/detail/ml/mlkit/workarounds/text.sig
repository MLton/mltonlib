(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature TEXT = sig
   structure Char : CHAR
   structure String : STRING
   structure Substring : SUBSTRING
   structure CharVector : MONO_VECTOR
   structure CharArray : MONO_ARRAY
   structure CharVectorSlice : MONO_VECTOR_SLICE
   structure CharArraySlice : MONO_ARRAY_SLICE
   sharing type Char.char
              = CharArray.elem
              = CharArraySlice.elem
              = CharVector.elem
              = CharVectorSlice.elem
              = String.char
              = Substring.char
   sharing type Char.string
              = CharArray.vector
              = CharArraySlice.vector
              = CharVector.vector
              = CharVectorSlice.vector
              = String.string
              = Substring.string
   sharing type CharArray.array
              = CharArraySlice.array
   sharing type CharArraySlice.vector_slice
              = CharVectorSlice.slice
end
