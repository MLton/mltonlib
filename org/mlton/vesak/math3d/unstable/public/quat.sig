(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature QUAT_CORE = sig
   structure Vec : VEC3
end

signature QUAT = sig
   include QUAT_CORE
   structure Scalar : SCALAR sharing Scalar = Vec.Scalar

   type t
   val t : t Generic.Rep.t

   val quat : {scalar : Scalar.t, vec : Vec.t} -> t

   val fromScalar : Scalar.t -> t
   val fromVec : Vec.t -> t

   val toRotMatrix :
       t -> {m11 : Scalar.t, m12 : Scalar.t, m13 : Scalar.t,
             m21 : Scalar.t, m22 : Scalar.t, m23 : Scalar.t,
             m31 : Scalar.t, m32 : Scalar.t, m33 : Scalar.t}

   val rot : {axis : Vec.t, rad : Scalar.t} -> t

   val rotX : {rad : Scalar.t} -> t
   val rotY : {rad : Scalar.t} -> t
   val rotZ : {rad : Scalar.t} -> t

   val scalar : t -> Scalar.t
   val vec : t -> Vec.t

   val one : t
   val zero : t

   val ~! : t UnOp.t

   val !+! : t BinOp.t  val !+ : t * Scalar.t -> t  val +! : Scalar.t * t -> t
   val !-! : t BinOp.t  val !- : t * Scalar.t -> t  val -! : Scalar.t * t -> t
   val !*! : t BinOp.t  val !* : t * Scalar.t -> t  val *! : Scalar.t * t -> t
   val !/! : t BinOp.t  val !/ : t * Scalar.t -> t  val /! : Scalar.t * t -> t

   val norm : t -> Scalar.t
   val mag : t -> Scalar.t
   val invMag : t -> Scalar.t

   val conj : t UnOp.t
   val inv : t UnOp.t

   val lerp : t Sq.t -> Scalar.t -> t
   val nlerp : t Sq.t -> Scalar.t -> t

   val normalize : t UnOp.t
end
