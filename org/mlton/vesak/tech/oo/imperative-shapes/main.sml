(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* A subtype polymorphic function on shapes: *)
fun drawAndMove s =
    (Shape.draw s
   ; Shape.rMoveTo s (100, 100)
   ; Shape.draw s)

(* Create some shapes: *)
val scribble = [Shape.partOf (Rectangle.new {x=10, y=20, w=5, h=6}),
                Shape.partOf (Circle.new {x=15, y=25, r=8})]

(* Handle shapes polymorphically: *)
val () = app drawAndMove scribble

(* Create a rectangle: *)
val rect = Rectangle.new {x=0, y=0, w=15, h=15}

(* Call a rectangle specific function: *)
val () = Rectangle.setW rect 30

(* Uses a Rectangle as a subtype of Shape: *)
val () = Shape.draw rect 
