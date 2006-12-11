signature REAL = sig

   structure Format: sig
      type t

      val exact: t
      val gen: Int.t -> t
      val fix: Int.t -> t
      val sci: Int.t -> t
   end

   structure Class: sig
      datatype t =
         Inf
       | Nan
       | Normal
       | Subnormal
       | Zero
   end

   structure Decimal: sig
      type t = {class: Class.t,
                digits: Int.t List.t,
                exp: Int.t,
                sign: Bool.t}

      val ofString: String.t -> t Option.t
      val scanner: t Scanner.t
      val toString: t -> String.t
   end

   structure RoundingMode: sig
      type t

      val get: Unit.t -> t
      val nearest: t
      val negInf: t
      val posInf: t
      val set: t -> Unit.t
      val zero: t
   end
   
   include ORDERED

   val != : t * t -> Bool.t
   val ?= : t * t -> Bool.t
   val + : t * t -> t
   val - : t * t -> t
   val * : t * t -> t
   val / : t * t -> t
   val ~ : t -> t
   val abs: t -> t
   val acos: t -> t
   val asin: t -> t
   val atan2: t * t -> t
   val atan: t -> t
   val ceil: t -> Int.t
   val checkFloat: t -> t
   val class: t -> Class.t
   val copySign: t * t -> t
   val cos: t -> t
   val cosh: t -> t
   val e: t
   val exp: t -> t
   val floor: t -> Int.t
   val format: t * Format.t -> String.t
   val isFinite: t -> Bool.t
   val isNan: t -> Bool.t
   val isNormal: t -> Bool.t
   val ln: t -> t
   val log10: t -> t
   val max: t * t -> t
   val maxFinite: t
   val min: t * t -> t
   val minNormalPos: t
   val minPos: t
   val negInf: t
   val nextAfter: t * t -> t
   val ofDecimal: Decimal.t -> t Option.t
   val ofInt: Int.t -> t
   val ofLarge: LargeReal.real * RoundingMode.t -> t
   val ofLargeInt: LargeInt.t -> t
   val ofManExp: {man: t, exp: Int.t} -> t
   val ofString: String.t -> t Option.t
   val pi: t
   val posInf: t
   val pow: t * t -> t
   val precision: Int.t
   val radix: Int.t
   val realCeil: t -> t
   val realFloor: t -> t
   val realMod: t -> t
   val realRound: t -> t
   val realTrunc: t -> t
   val rem: t * t -> t
   val round: t -> Int.t
   val sameSign: t * t -> Bool.t
   val scanner: t Scanner.t
   val sign: t -> Int.t
   val signBit: t -> Bool.t
   val sin: t -> t
   val sinh: t -> t
   val split: t -> {frac: t, whole: t}
   val sqrt: t -> t
   val tan: t -> t
   val tanh: t -> t 
   val toDecimal: t -> Decimal.t
   val toInt: t * RoundingMode.t -> Int.t
   val toLarge: t -> LargeReal.real
   val toLargeInt: t * RoundingMode.t -> LargeInt.t
   val toManExp: t -> {man: t, exp: Int.t}
   val toString: t -> String.t
   val trunc: t -> Int.t
   val unordered: t * t -> Bool.t
   
end
