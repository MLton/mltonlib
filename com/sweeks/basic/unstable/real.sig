signature REAL = sig

   structure Format: sig
      type t

      val exact: t
      val gen: int -> t
      val fix: int -> t
      val sci: int -> t
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
                digits: int list,
                exp: int,
                sign: bool}

      val ofString: string -> t option
      val scanner: t Scanner.t
      val toString: t -> string
   end

   structure RoundingMode: sig
      type t

      val get: unit -> t
      val nearest: t
      val negInf: t
      val posInf: t
      val set: t -> unit
      val zero: t
   end
   
   include ORDERED

   val != : t * t -> bool
   val ?= : t * t -> bool
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
   val ceil: t -> int
   val checkFloat: t -> t
   val class: t -> Class.t
   val copySign: t * t -> t
   val cos: t -> t
   val cosh: t -> t
   val e: t
   val exp: t -> t
   val floor: t -> int
   val format: t * Format.t -> string
   val isFinite: t -> bool
   val isNan: t -> bool
   val isNormal: t -> bool
   val ln: t -> t
   val log10: t -> t
   val max: t * t -> t
   val maxFinite: t
   val min: t * t -> t
   val minNormalPos: t
   val minPos: t
   val negInf: t
   val nextAfter: t * t -> t
   val ofDecimal: Decimal.t -> t option
   val ofInt: int -> t
   val ofLarge: LargeReal.real * RoundingMode.t -> t
   val ofLargeInt: LargeInt.t -> t
   val ofManExp: {man: t, exp: int} -> t
   val ofString: string -> t option
   val pi: t
   val posInf: t
   val pow: t * t -> t
   val precision: int
   val radix: int
   val realCeil: t -> t
   val realFloor: t -> t
   val realMod: t -> t
   val realRound: t -> t
   val realTrunc: t -> t
   val rem: t * t -> t
   val round: t -> int
   val sameSign: t * t -> bool
   val scanner: t Scanner.t
   val sign: t -> int
   val signBit: t -> bool
   val sin: t -> t
   val sinh: t -> t
   val split: t -> {frac: t, whole: t}
   val sqrt: t -> t
   val tan: t -> t
   val tanh: t -> t 
   val toDecimal: t -> Decimal.t
   val toInt: t * RoundingMode.t -> int
   val toLarge: t -> LargeReal.real
   val toLargeInt: t * RoundingMode.t -> LargeInt.t
   val toManExp: t -> {man: t, exp: int}
   val toString: t -> string
   val trunc: t -> int
   val unordered: t * t -> bool
   
end
