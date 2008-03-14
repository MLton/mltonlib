(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkVectorSequence (ElemVector : MONO_VECTOR) :>
   VECTOR_SEQUENCE
      where type Pos.t = Int.t
      where type ElemVector.elem = ElemVector.elem
      where type ElemVector.t = ElemVector.t =
struct
   structure Pos = Int
   structure Elem = struct type t = ElemVector.elem end
   structure ElemVector = ElemVector
   type t = {pos : Pos.t, data : ElemVector.t}
   fun full s : t = {pos = 0, data = s}
   val pos : t -> Pos.t = #pos
   val vector : t -> ElemVector.t = #data
   val get : (Elem.t, t) Reader.t =
    fn {pos, data} =>
       if pos < ElemVector.length data
       then SOME (ElemVector.sub (data, pos), {pos = pos+1, data = data})
       else NONE
end

structure StringSequence = MkVectorSequence (CharVector)
structure Word8VectorSequence = MkVectorSequence (Word8Vector)
