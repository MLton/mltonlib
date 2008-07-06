(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkRBT (Arg : RBT_CORE) : RBT = struct
   open Arg open Rot open Vec open Scalar

   datatype t = RBT of {r : Rot.t, t : Vec.t}
   fun out (RBT r) = r

   val t =
       data' (C1'"RBT" (record (R'"r" Rot.t *` R'"t" Vec.t)))
             (fn (RBT {r=r, t=t}) => (r & t),
              fn (r & t) => (RBT {r=r, t=t}))

   val identity = RBT {r = Rot.identity, t = zero}

   fun rbt {rotation, translation} = RBT {r=rotation, t=translation}

   val rotation = #r o out
   val translation = #t o out

   fun fromRotation r = RBT {r = r, t = zero}
   fun fromTranslation t = RBT {r = Rot.identity, t = t}

   fun toMatrix (RBT {r, t}) = let
      val {m11, m12, m13,
           m21, m22, m23,
           m31, m32, m33} = toRotMatrix r
      val s0 = fromInt 0 val s1 = fromInt 1
      val {x, y, z} = toXYZ (toSeq t)
   in
      {m11=m11, m12=m12, m13=m13, m14=x,
       m21=m21, m22=m22, m23=m23, m24=y,
       m31=m31, m32=m32, m33=m33, m34=z,
       m41=s0,  m42=s0,  m43=s0,  m44=s1}
   end

   fun transform (RBT {r, t}) =
       case Rot.transform r of r => fn p => r p |+| t

   val rotate = Rot.transform o rotation
   fun translate rbt = translation rbt <\ op |+|

   fun (RBT l) %*% (RBT r) =
       RBT {r = #r l @*@ #r r,
            t = #t l |+| Rot.transform (#r l) (#t r)}

   fun inv (RBT {r, t}) =
       case Rot.inv r of r => RBT {r = r, t = Rot.transform r (~|t)}
end
