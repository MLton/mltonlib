(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure RealStructs = struct
   
   structure Class = struct

      datatype t =
         Inf
       | Nan
       | Normal
       | Subnormal
       | Zero

      local
         datatype z = datatype Basis.IEEEReal.float_class
      in
         val ofBasis =
            fn INF => Inf
             | NAN => Nan
             | NORMAL => Normal
             | SUBNORMAL => Subnormal
             | ZERO => Zero
         val toBasis =
            fn Inf => INF
             | Nan => NAN
             | Normal => NORMAL
             | Subnormal => SUBNORMAL
             | Zero => ZERO
      end

   end

   structure Decimal = struct

      type 'a u = {class: 'a,
                   digits: Int.t List.t,
                   exp: Int.t,
                   sign: Bool.t}

      type t = Class.t u

      local
         fun make f ({class, digits, exp, sign}: 'a u) =
            {class = f class,
             digits = digits,
             exp = exp,
             sign = sign}
      in
         val ofBasis = make Class.ofBasis
         val toBasis = make Class.toBasis
      end

      val toString = IEEEReal.toString o toBasis

      val scanner = Scanner.map (Scanner.ofBasis IEEEReal.scan, ofBasis)

      fun ofString s = Scanner.scanString (scanner, s)

   end

   structure RoundingMode = struct

      open Basis.IEEEReal
         
      datatype t = datatype rounding_mode

      val get = getRoundingMode
      val nearest = TO_NEAREST
      val negInf = TO_NEGINF
      val posInf = TO_POSINF
      val set = setRoundingMode
      val zero = TO_ZERO         
   end

   structure Format = struct
      datatype t = datatype Basis.StringCvt.realfmt

      val exact = EXACT
      val fix = FIX o SOME
      val gen = GEN o SOME
      val sci = SCI o SOME
   end

end
