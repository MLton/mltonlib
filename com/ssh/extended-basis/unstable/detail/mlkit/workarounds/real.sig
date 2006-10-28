(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REAL = sig
   include REAL
   val fromDecimal : IEEEReal.decimal_approx -> real option
   val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real
   val fromLargeInt : LargeInt.int -> real
   val fromManExp : {man : real, exp : int} -> real
   val toDecimal : real -> IEEEReal.decimal_approx
   val toInt : IEEEReal.rounding_mode -> real -> int
   val toLarge : real -> LargeReal.real
   val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int
   val toManExp : real -> {man : real, exp : int}
end
