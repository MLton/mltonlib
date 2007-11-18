(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Circle : sig
   include CIRCLE
   val new : {x : Int.t, y : Int.t, r : Int.t} -> Unit.t t
end = struct
   structure D = Sub (open Shape type x = {r : Int.t Var.t})
   open Shape D
   fun getR c = #get (its#r c) ()
   fun setR c = #set (its#r c)
   fun new {x, y, r} = let
      val x = Var.new x and y = Var.new y
      val r = Var.new r
      fun draw () =
          print (concat ["Drawing a Circle at:(", Int.toString (#get x ()), ",",
                         Int.toString (#get y ()), "), Radius ",
                         Int.toString (#get r ()), "\n"])
   in
      () & {r = r} & {x = x, y = y, draw = draw}
   end
end
