(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Plane == *)

signature PLANE_CORE = sig
   structure RBT : RBT
end

signature PLANE = sig
   include PLANE_CORE
   structure Rot : ROT sharing Rot = RBT.Rot
   structure Scalar : SCALAR sharing Scalar = RBT.Scalar
   structure Vec : VEC3 sharing Vec = RBT.Vec

   type t
   val t : t Generic.Rep.t

   val plane : {normal : Vec.t, distance : Scalar.t} -> t

   val normal : t -> Vec.t
   val distance : t -> Scalar.t

   val translate : Vec.t -> t UnOp.t
   val rotate : Rot.t -> t UnOp.t
   val transform : RBT.t -> t UnOp.t
end
