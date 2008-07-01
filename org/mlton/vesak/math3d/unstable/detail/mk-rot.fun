(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRot (Arg : ROT_CORE) : ROT = struct
   open Arg open Quat open Scalar open Math

   datatype t = ROT of Quat.t
   fun out (ROT q) = q
   val t = let open Generic in data' (C1'"ROT" Quat.t) (out, ROT) end

   val identity = ROT one

   val axis = vec o out
   val rad = undefined

   val eps = nextAfter (fromInt 1, fromInt 2) - fromInt 1
   val sqrtEps = sqrt eps

   val fromQuat = ROT o Quat.normalize
   val toQuat = out

   val rot = ROT o rot

   val rotX = ROT o rotX
   val rotY = ROT o rotY
   val rotZ = ROT o rotZ

   fun toRotMatrix (ROT q) = let
      val s = Quat.scalar q
      val v = Quat.vec q
      val {x, y, z} = Vec.toXYZ (Vec.toSeq v)
      val k = fromInt 2
      val kx = k*x val ky = k*y val kz = k*z
      val xkx = x*kx val yky = y*ky val zkz = z*kz
      val i = fromInt 1
      val xky = x*ky val ykz = y*kz val zkx = z*kx
      val skx = s*kx val sky = s*ky val skz = s*kz
   in
      {m11 = i-(yky+zkz), m12 =   (xky-skz), m13 =   (zkx+sky),
       m21 =   (xky+skz), m22 = i-(zkz+xkx), m23 =   (ykz-skx),
       m31 =   (zkx-sky), m32 =   (ykz+skx), m33 = i-(xkx+yky)}
   end

   fun toMatrix r = let
      val {m11, m12, m13,
           m21, m22, m23,
           m31, m32, m33} = toRotMatrix r
      val s0 = fromInt 0
      val s1 = fromInt 1
   in
      {m11 = m11, m12 = m12, m13 = m13, m14 = s0,
       m21 = m21, m22 = m22, m23 = m23, m24 = s0,
       m31 = m31, m32 = m32, m33 = m33, m34 = s0,
       m41 = s0,  m42 = s0,  m43 = s0,  m44 = s1}
   end

   fun transform r =
       case toRotMatrix r
        of {m11, m12, m13,
            m21, m22, m23,
            m31, m32, m33} =>
           fn p =>
              case Vec.toXYZ (Vec.toSeq p)
               of {x, y, z} =>
                  Vec.fromSeq
                   (Vec.fromXYZ
                     {x = m11*x + m12*y + m13*z,
                      y = m21*x + m22*y + m23*z,
                      z = m31*x + m32*y + m33*z})

   fun renormalize q = let
      val n = norm q
   in
      if abs (n - fromInt 1) < sqrtEps
      then q
      else q !* fromInt 1 / sqrt n
   end

   fun (ROT l) @*@ (ROT r) = ROT (renormalize (l !*! r))

   val inv = ROT o conj o out

   fun lerp (ROT l, ROT r) = ROT o nlerp (l, r)
end
