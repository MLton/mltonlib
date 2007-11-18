(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Shape : SHAPE = struct
   structure D = Sub (open Any
                      type x = {x : Int.t Var.t,
                                y : Int.t Var.t,
                                draw : Unit.t Effect.t})
   open Any D
   fun getX s = #get (its#x s) ()
   fun getY s = #get (its#y s) ()
   fun setX s = #set (its#x s)
   fun setY s = #set (its#y s)
   fun draw s = its#draw s ()
   fun moveTo s (x, y) = (setX s x ; setY s y)
   fun rMoveTo s (dx, dy) = moveTo s (getX s + dx, getY s + dy)
end
