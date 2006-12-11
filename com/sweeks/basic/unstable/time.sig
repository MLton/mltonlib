signature TIME = sig

   include ORDERED

   exception Time

   val + : t * t -> t
   val - : t * t -> t
   val format: t * {fractionalDigits: Int.t} -> String.t
   val fromReal: LargeReal.t -> t
   val now: Unit.t -> t
   val ofMicroseconds: LargeInt.t -> t
   val ofMilliseconds: LargeInt.t -> t
   val ofNanoseconds: LargeInt.t -> t
   val ofSeconds: LargeInt.t -> t
   val ofString: String.t -> t Option.t
   val scanner: t Scanner.t
   val toMicroseconds: t -> LargeInt.t
   val toMilliseconds: t -> LargeInt.t
   val toNanoseconds: t -> LargeInt.t
   val toReal: t -> LargeReal.t
   val toSeconds: t -> LargeInt.t
   val toString: t -> String.t
   val zero: t

end
