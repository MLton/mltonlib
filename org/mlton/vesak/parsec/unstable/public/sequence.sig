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

signature STRING_SEQUENCE = sig
   include SEQUENCE
      where type Pos.t = Int.t
      where type Elem.t = Char.t
   val full : String.t -> t
   val string : t -> String.t
end
