(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

local
   open Generic
in
   structure Real32 = struct
      open Real32
      val t = iso largeReal (Real32.isoLarge IEEEReal.TO_NEAREST)
   end

   structure Real64 = struct open Real64 val t = real end
   structure Int64 = struct open Int64 val t = fixedInt end
   structure IntInf = struct open IntInf val t = largeInt end
end
