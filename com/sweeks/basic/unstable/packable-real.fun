(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
functor PackableReal
   (structure PackBig: BASIS_PACK_REAL
    structure PackLittle: BASIS_PACK_REAL
    structure Real: BASIS_REAL
    sharing type PackBig.real = PackLittle.real = Real.real)
   : PACKABLE_REAL = struct

   structure Real = Real (structure Real = Real)

   open Real

   fun switch (e, (b, l)) =
      case e of
         Endian.Big => b
       | Endian.Little => l
            
   local
      fun make fs (x, e) = (switch (e, fs)) x
   in
      val ofBytes = make (PackBig.fromBytes, PackLittle.fromBytes)
      val toBytes = make (PackBig.toBytes, PackLittle.toBytes)
   end

   local
      fun make fs (x, y, e) = (switch (e, fs)) (x, y)
   in
      val subArr = make (PackBig.subArr, PackLittle.subArr)
      val subVec = make (PackBig.subVec, PackLittle.subVec)
   end

   fun update (a, i, r, e) =
      (switch (e, (PackBig.update, PackLittle.update))) (a, i, r)

end
