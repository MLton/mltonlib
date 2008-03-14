(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SEQUENCE = sig
   type t
   structure Elem : T
   structure Pos : T
   val pos : t -> Pos.t
   val get : (Elem.t, t) Reader.t
end

signature VECTOR_SEQUENCE = sig
   include SEQUENCE
   structure ElemVector : MONO_VECTOR
   sharing type Elem.t = ElemVector.elem
   val full : ElemVector.t -> t
   val vector : t -> ElemVector.t
end
