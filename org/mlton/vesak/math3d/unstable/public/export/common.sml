(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature PLANE = PLANE
signature QUAT = QUAT
signature RBT = RBT
signature ROT = ROT
signature SEQ = SEQ
signature VEC = VEC
signature VEC3 = VEC3

(** == Exported Functors == *)

signature SEQ_CORE = SEQ_CORE
functor MkSeq (Arg : SEQ_CORE) : SEQ = MkSeq (Arg)

signature VEC_CORE = VEC_CORE
functor MkVec (Arg : VEC_CORE) : VEC = MkVec (Arg)

signature QUAT_CORE = QUAT_CORE
functor MkQuat (Arg : QUAT_CORE) : QUAT = MkQuat (Arg)

signature ROT_CORE = ROT_CORE
functor MkRot (Arg : ROT_CORE) : ROT = MkRot (Arg)

signature RBT_CORE = RBT_CORE
functor MkRBT (Arg : RBT_CORE) : RBT = MkRBT (Arg)

signature PLANE_CORE = PLANE_CORE
functor MkPlane (Arg : PLANE_CORE) : PLANE = MkPlane (Arg)

(** == Exported Structures == *)

structure SeqXY : SEQ = SeqXY
structure SeqXYZ : SEQ = SeqXYZ
structure SeqXYZW : SEQ = SeqXYZW

structure Vec2D : VEC = Vec2D
structure Vec3D : VEC3 = Vec3D
structure Vec4D : VEC = Vec4D

structure QuatD : QUAT = QuatD
structure RotD : ROT = RotD
structure RBTD : RBT = RBTD

structure PlaneD : PLANE = PlaneD
