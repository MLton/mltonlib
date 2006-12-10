(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Extended {TEXT} signature.
 *
 * This signature is just a restament of the {TEXT} signature using the
 * extended substructure signatures.
 *)
signature TEXT = sig
   structure Char            : CHAR
   structure CharArray       : MONO_ARRAY
   structure CharArraySlice  : MONO_ARRAY_SLICE
   structure CharVector      : MONO_VECTOR
   structure CharVectorSlice : MONO_VECTOR_SLICE
   structure String          : STRING
   structure Substring       : SUBSTRING
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
