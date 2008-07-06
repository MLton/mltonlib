(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkQuat (Arg : QUAT_CORE) : QUAT = struct
   open Arg open Vec open Scalar open Math

   datatype t = QUAT of {s : Scalar.t, v : Vec.t}
   fun out (QUAT r) = r

   val t =
       data' (C1'"QUAT" (record (R'"s" Scalar.t *` R'"v" Vec.t)))
             (fn QUAT {s=s, v=v} => s & v,
              fn s & v => QUAT {s=s, v=v})

   val scalar = #s o out
   val vec = #v o out

   fun quat {scalar, vec} = QUAT {s=scalar, v=vec}

   fun fromScalar s = QUAT {s = s, v = Vec.zero}
   fun fromVec v = QUAT {s = fromInt 0, v = v}

   fun rot {axis, rad} =
       QUAT {s = cos rad,
             v = axis |* invMag axis * sin rad} (* XXX Vec.withMag *)

   val {x=rotX, y=rotY, z=rotZ} =
       toXYZ (Seq.map (fn axis => fn {rad} =>
                          QUAT {s = cos rad, v = update axis (zero, sin rad)})
                      Seq.selector)

   val one = fromScalar (fromInt 1)
   val zero = fromScalar (fromInt 0)

   fun ~! (QUAT {s, v}) = QUAT {s = ~s, v = ~|v}

   fun norm (QUAT {s, v}) = Scalar.sq s + Vec.norm v
   val mag = sqrt o norm
   val invMag = fromInt 1 <\ op / o mag (* XXX invSqrt *)

   fun conj (QUAT {s, v}) = QUAT {s = s, v = ~|v}
   fun inv (q as QUAT {s, v}) =
       case fromInt 1 / norm q
        of k => QUAT {s = k * s, v = ~k *| v}

   local
      fun mk op + op |+| =
          case fn (l, r) => QUAT {s = scalar l + r, v = vec l}
           of qxs =>
              (fn (l, r) =>
                  QUAT {s = scalar l + scalar r, v = vec l |+| vec r},
               qxs, qxs o swap)
   in
      val (op !+!, op !+, op +!) = mk op + op |+|
      val (op !-!, op !-, op -!) = mk op - op |-|
   end

   fun l !*! r =
       QUAT {s = scalar l * scalar r - dot (vec l, vec r),
             v = cross (vec l, vec r)
                  |+| scalar r *| vec l
                  |+| scalar l *| vec r}
   fun (QUAT {s, v}) !* k = QUAT {s = s * k, v = v |* k}
   val op *! = op !* o swap

   fun l !/! r = l !*! inv r
   fun l !/ r = l !* fromInt 1 / r
   fun l /! r = l *! inv r

   val eps = nextAfter (fromInt 1, fromInt 2) - fromInt 1

   fun normalize q = let
      val n = norm q
   in
      if n < eps then zero else q !* fromInt 1 / sqrt n (* XXX invSqrt *)
   end

   fun lerp (l, r) t = l !* (fromInt 1 - t) !+! r !* t
   fun nlerp (l, r) = normalize o lerp (l, r)

   fun toRotMatrix (q as QUAT {s, v}) = let
      val {x, y, z} = Vec.toXYZ (Vec.toSeq v)
      val k = fromInt 2 / norm q
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
end
