(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure StringSequence :> STRING_SEQUENCE = struct
   structure Pos = Int
   structure Elem = Char
   type t = {pos : Pos.t, data : String.t}
   fun full s : t = {pos = 0, data = s}
   val pos : t -> Pos.t = #pos
   val string : t -> String.t = #data
   val get : (Elem.t, t) Reader.t =
    fn {pos, data} =>
       if pos < size data
       then SOME (String.sub (data, pos), {pos = pos+1, data = data})
       else NONE
end
