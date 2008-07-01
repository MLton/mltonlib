(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic
   val x = L"x" val y = L"y" val z = L"z" val w = L"w"
in
   structure SeqXY =
      MkSeq (type 'a t = {x : 'a, y : 'a}
             fun t t =
                 record' (R x t *` R y t)
                         (fn {x=x, y=y} => (x & y),
                          fn (x & y) => {x=x, y=y})
             val selector : ('a t -> 'a) t = {x = #x, y = #y}
             fun map f {x, y} = {x = f x, y = f y}
             fun foldr f s {x, y} = f (x, f (y, s))
             fun findSome f {x, y} =
                 case f x of SOME r => SOME r | NONE =>
                 f y)

   structure SeqXYZ =
      MkSeq (type 'a t = {x : 'a, y : 'a, z : 'a}
             fun t t =
                 record' (R x t *` R y t *` R z t)
                         (fn {x=x, y=y, z=z} => (x & y & z),
                          fn (x & y & z) => {x=x, y=y, z=z})
             val selector : ('a t -> 'a) t = {x = #x, y = #y, z = #z}
             fun map f {x, y, z} = {x = f x, y = f y, z = f z}
             fun foldr f s {x, y, z} = f (x, f (y, f (z, s)))
             fun findSome f {x, y, z} =
                 case f x of SOME r => SOME r | NONE =>
                 case f y of SOME r => SOME r | NONE =>
                 f z)

   structure SeqXYZW =
      MkSeq (type 'a t = {x : 'a, y : 'a, z : 'a, w : 'a}
             fun t t =
                 record' (R x t *` R y t *` R z t *` R w t)
                         (fn {x=x, y=y, z=z, w=w} => (x & y & z & w),
                          fn (x & y & z & w) => {x=x, y=y, z=z, w=w})
             val selector : ('a t -> 'a) t = {x = #x, y = #y, z = #z, w = #w}
             fun map f {x, y, z, w} = {x = f x, y = f y, z = f z, w = f w}
             fun foldr f s {x, y, z, w} = f (x, f (y, f (z, f (w, s))))
             fun findSome f {x, y, z, w} =
                 case f x of SOME r => SOME r | NONE =>
                 case f y of SOME r => SOME r | NONE =>
                 case f z of SOME r => SOME r | NONE =>
                 f w)
end

structure Vec2D = MkVec (structure Scalar = Real and Seq = SeqXY)
structure Vec3D = MkVec (structure Scalar = Real and Seq = SeqXYZ)
structure Vec4D = MkVec (structure Scalar = Real and Seq = SeqXYZW)

structure Vec3D = struct
   open Vec3D
   structure Vec4 = Vec4D
   val toXYZ = id
   val fromXYZ = id
   fun cross (a : t, b : t) = let
      open Scalar
      fun el u v = u a * v b - u b * v a
   in
      {x = el #y #z, y = el #z #x, z = el #x #y}
   end
   fun toVec4 w {x, y, z} : Vec4D.t = {x=x, y=y, z=z, w=w}
   fun fromVec4 {x, y, z, w=_} : t = {x=x, y=y, z=z}
end

structure QuatD = MkQuat (structure Vec = Vec3D)
structure RotD = MkRot (structure Quat = QuatD)
structure RBTD = MkRBT (structure Rot = RotD)

structure PlaneD = MkPlane (structure RBT = RBTD)
