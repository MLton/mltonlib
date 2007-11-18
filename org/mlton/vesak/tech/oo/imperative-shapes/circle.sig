(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature CIRCLE = sig
   include SHAPE
   val getR : 'a t -> Int.t
   val setR : 'a t -> Int.t Effect.t
end
