(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkPlane (Arg : PLANE_CORE) : PLANE = struct
   open Arg open RBT open Vec open Scalar open Math

   datatype t = PLANE of {n : Vec.t, d : Scalar.t}
   val t = let
      open Generic
   in
      data' (C1'"PLANE" (record (R'"n" Vec.t *` R'"d" Scalar.t)))
            (fn (PLANE {n=n, d=d}) => (n & d),
             fn (n & d) => (PLANE {n=n, d=d}))
   end

   fun out (PLANE r) = r
   val normal = #n o out
   val distance = #d o out

   val eps = nextAfter (fromInt 1, fromInt 2) - fromInt 1
   val sqrtEps = sqrt eps

   fun renormalize' nn (PLANE {n, d}) =
       if abs (nn - fromInt 1) < sqrtEps
       then PLANE {n = n, d = d}
       else case sqrt nn of m => PLANE {n = n |* fromInt 1 / m, d = d * m}

   fun renormalize (plane as PLANE {n, ...}) =
       renormalize' (norm n) plane

   fun plane {normal, distance} =
       case norm normal
        of n => if n < eps
                then fail "Plane.plane"
                else renormalize' n (PLANE {n=normal, d=distance})

   fun translate translation (PLANE {n, d}) =
       PLANE {n = n, d = d + dot (translation, n)}

   fun rotate rot =
       case Rot.transform rot
        of rotate =>
           fn PLANE {n, d} => renormalize (PLANE {n = rotate n, d = d})

   fun transform rbt =
       translate (RBT.translation rbt) o rotate (RBT.rotation rbt)
end
