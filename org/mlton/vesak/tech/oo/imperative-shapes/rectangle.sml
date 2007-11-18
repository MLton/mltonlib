(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Rectangle : sig
   include RECTANGLE
   val new : {x : Int.t, y : Int.t, w : Int.t, h : Int.t} -> Unit.t t
end = struct
   structure D = Sub (open Shape type x = {w : Int.t Var.t, h : Int.t Var.t})
   open Shape D
   fun getW r = #get (its#w r) ()
   fun getH r = #get (its#h r) ()
   fun setW r = #set (its#w r)
   fun setH r = #set (its#h r)
   fun new {x, y, w, h} = let
      val x = Var.new x and y = Var.new y
      val w = Var.new w and h = Var.new h
      fun draw () =
          print (concat ["Drawing a Rectangle at:(", Int.toString (#get x ()),
                         ",", Int.toString (#get y ()), "), Width ",
                         Int.toString (#get w ()), ", Height ",
                         Int.toString (#get h ()), "\n"])
   in
      () & {w = w, h = h} & {x = x, y = y, draw = draw}
   end
end
