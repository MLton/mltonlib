(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature SHAPE = sig
   include ANY
   val getX : 'a t -> Int.t
   val getY : 'a t -> Int.t
   val setX : 'a t -> Int.t Effect.t
   val setY : 'a t -> Int.t Effect.t
   val draw : 'a t Effect.t
   val moveTo : 'a t -> Int.t Sq.t Effect.t
   val rMoveTo : 'a t -> Int.t Sq.t Effect.t
end
