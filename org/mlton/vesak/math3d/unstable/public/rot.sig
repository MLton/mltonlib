(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature ROT_CORE = sig
   structure Quat : QUAT
end

signature ROT = sig
   include ROT_CORE
   structure Scalar : SCALAR sharing Scalar = Quat.Scalar
   structure Vec : VEC3 sharing Vec = Quat.Vec

   type t
   val t : t Rep.t

   val identity : t

   val axis : t -> Vec.t
   val rad : t -> Scalar.t

   val fromQuat : Quat.t -> t
   val toQuat : t -> Quat.t

   val rot : {axis : Vec.t, rad : Scalar.t} -> t

   val rotX : {rad : Scalar.t} -> t
   val rotY : {rad : Scalar.t} -> t
   val rotZ : {rad : Scalar.t} -> t

   val toRotMatrix :
       t -> {m11 : Scalar.t, m12 : Scalar.t, m13 : Scalar.t,
             m21 : Scalar.t, m22 : Scalar.t, m23 : Scalar.t,
             m31 : Scalar.t, m32 : Scalar.t, m33 : Scalar.t}

   val toMatrix :
       t -> {m11 : Scalar.t, m12 : Scalar.t, m13 : Scalar.t, m14 : Scalar.t,
             m21 : Scalar.t, m22 : Scalar.t, m23 : Scalar.t, m24 : Scalar.t,
             m31 : Scalar.t, m32 : Scalar.t, m33 : Scalar.t, m34 : Scalar.t,
             m41 : Scalar.t, m42 : Scalar.t, m43 : Scalar.t, m44 : Scalar.t}

   val transform : t -> Vec.t UnOp.t

   val @*@ : t BinOp.t

   val inv : t UnOp.t

   val lerp : t Sq.t -> Scalar.t -> t
end
