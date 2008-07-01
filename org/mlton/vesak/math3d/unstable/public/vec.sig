(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature VEC_CORE = sig
   structure Scalar : SCALAR
   structure Seq : SEQ
end

signature VEC = sig
   include VEC_CORE

   type t (* = Scalar.t Seq.t *)
   val t : t Generic.Rep.t

   val fromSeq : Scalar.t Seq.t -> t
   val toSeq : t -> Scalar.t Seq.t

   val sub : (Scalar.t Seq.r Seq.t -> Scalar.t Seq.r) -> t -> Scalar.t
   val update : (Scalar.t Seq.r Seq.t -> Scalar.t Seq.r) -> t * Scalar.t -> t

   val diag : Scalar.t -> t -> t Seq.t

   val zero : t

   val e : t Seq.t

   val ~| : t UnOp.t

   val |+| : t BinOp.t  val |+ : t * Scalar.t -> t  val +| : Scalar.t * t -> t
   val |-| : t BinOp.t  val |- : t * Scalar.t -> t  val -| : Scalar.t * t -> t
   val |*| : t BinOp.t  val |* : t * Scalar.t -> t  val *| : Scalar.t * t -> t
   val |/| : t BinOp.t  val |/ : t * Scalar.t -> t  val /| : Scalar.t * t -> t

   val |<=| : t BinPr.t
   val |<|  : t BinPr.t
   val |>=| : t BinPr.t
   val |>|  : t BinPr.t

   val minElems : t BinOp.t
   val maxElems : t BinOp.t

   val dot : t Sq.t -> Scalar.t
   val norm : t -> Scalar.t
   val mag : t -> Scalar.t
   val invMag : t -> Scalar.t

   val lerp : t Sq.t -> Scalar.t -> t
   val nlerp : t Sq.t -> Scalar.t -> t

   val normalize : t UnOp.t
end

signature VEC3 = sig
   include VEC
   structure Vec4 : VEC
   val toXYZ : 'a Seq.t -> {x : 'a, y : 'a, z : 'a}
   val fromXYZ : {x : 'a, y : 'a, z : 'a} -> 'a Seq.t
   val cross : t BinOp.t
   val toVec4 : Scalar.t -> t -> Vec4.t
   val fromVec4 : Vec4.t -> t
end
