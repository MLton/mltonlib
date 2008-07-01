(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Vec2F = MkVec (structure Scalar = Real32 and Seq = SeqXY)
structure Vec3F = MkVec (structure Scalar = Real32 and Seq = SeqXYZ)
structure Vec4F = MkVec (structure Scalar = Real32 and Seq = SeqXYZW)

structure Vec3F = struct
   open Vec3F
   structure Vec4 = Vec4F
   val toXYZ = id
   val fromXYZ = id
   fun cross (a : t, b : t) = let
      open Scalar
      fun el u v = u a * v b - u b * v a
   in
      {x = el #y #z, y = el #z #x, z = el #x #y}
   end
   fun toVec4 w {x, y, z} : Vec4F.t = {x=x, y=y, z=z, w=w}
   fun fromVec4 {x, y, z, w=_} : t = {x=x, y=y, z=z}
end

structure QuatF = MkQuat (structure Vec = Vec3F)
structure RotF = MkRot (structure Quat = QuatF)
structure RBTF = MkRBT (structure Rot = RotF)
structure PlaneF = MkPlane (structure RBT = RBTF)
