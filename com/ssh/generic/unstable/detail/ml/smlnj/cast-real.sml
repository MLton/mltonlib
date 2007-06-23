(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CastReal : CAST_REAL where type t = Real.t = struct
   type t = Real64.t
   structure Word = Word64
   local
      fun cast {size=sizeF, set=setF, get=_   }
               {size=sizeT, set=_,    get=getT} vF =
          if C.S.toWord sizeF <> C.S.toWord sizeT then
             raise Fail "CastReal: sizes do not match"
          else let
                val objF = C.new' sizeF
                val objT =
                    let open C.Ptr in |*! (cast' (inject' (|&! objF))) end
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
      val castToWord   = cast real64 word64
      val castFromWord = cast word64 real64
   end
end

structure CastLargeReal : CAST_REAL where type t = LargeReal.t = CastReal
