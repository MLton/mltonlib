(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature RECTANGLE = sig
   include SHAPE
   val getH : 'a t -> Int.t
   val getW : 'a t -> Int.t
   val setH : 'a t -> Int.t Effect.t
   val setW : 'a t -> Int.t Effect.t
end
