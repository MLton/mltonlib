(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Rigid-Body Transform == *)

signature RBT_CORE = sig
   structure Rot : ROT
end

signature RBT = sig
   include RBT_CORE
   structure Quat : QUAT sharing Quat = Rot.Quat
   structure Scalar : SCALAR sharing Scalar = Rot.Scalar
   structure Vec : VEC3 sharing Vec = Rot.Vec

   type t
   val t : t Generic.Rep.t

   val identity : t

   val rbt : {rotation : Rot.t, translation : Vec.t} -> t

   val fromRotation : Rot.t -> t
   val fromTranslation : Vec.t -> t

   val toMatrix :
       t -> {m11 : Scalar.t, m12 : Scalar.t, m13 : Scalar.t, m14 : Scalar.t,
             m21 : Scalar.t, m22 : Scalar.t, m23 : Scalar.t, m24 : Scalar.t,
             m31 : Scalar.t, m32 : Scalar.t, m33 : Scalar.t, m34 : Scalar.t,
             m41 : Scalar.t, m42 : Scalar.t, m43 : Scalar.t, m44 : Scalar.t}

   val transform : t -> Vec.t UnOp.t
   val rotate : t -> Vec.t UnOp.t
   val translate : t -> Vec.t UnOp.t

   val rotation : t -> Rot.t
   val translation : t -> Vec.t

   val %*% : t BinOp.t

   val inv : t UnOp.t
end
