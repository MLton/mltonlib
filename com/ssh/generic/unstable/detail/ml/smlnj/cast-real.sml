(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CastReal : CAST_REAL where type t = Real.t = struct
   type t = Real64.t
   structure Bits = Word64
   local
      fun cast {size=sizeF, set=setF, get=_   }
               {size=sizeT, set=_,    get=getT} =
          if C.S.toWord sizeF <> C.S.toWord sizeT then
             raise Fail "CastReal: sizes do not match"
          else
             fn vF => let
                   open C.Ptr
                   val objF = C.new' sizeF
                   val objT = |*! (cast' (inject' (|&! objF)))
                in
                   setF (objF, vF)
                 ; getT objT before C.discard' objF
                end
      val word64 = {size = C.S.ulonglong,
                    set = C.Set.ulonglong',
                    get = C.Get.ulonglong'}
      val real64 = {size = C.S.double,
                    set = C.Set.double',
                    get = C.Get.double'}
   in
      val isoBits = SOME (cast real64 word64, cast word64 real64)
   end
end

structure CastLargeReal : CAST_REAL where type t = LargeReal.t = CastReal
