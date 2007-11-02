(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CastReal : CAST_REAL where type t = Real.t = struct
   open Real
   structure Bits = Word
   val isoBits = NONE
end

structure CastLargeReal : CAST_REAL where type t = LargeReal.t = CastReal
