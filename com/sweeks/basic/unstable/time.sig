signature TIME = sig

   include ORDERED

   exception Time

   val + : t * t -> t
   val - : t * t -> t
   val format: t * {fractionalDigits: int} -> string
   val fromReal: LargeReal.t -> t
   val now: unit -> t
   val ofMicroseconds: LargeInt.t -> t
   val ofMilliseconds: LargeInt.t -> t
   val ofNanoseconds: LargeInt.t -> t
   val ofSeconds: LargeInt.t -> t
   val ofString: string -> t option
   val scanner: t Scanner.t
   val toMicroseconds: t -> LargeInt.t
   val toMilliseconds: t -> LargeInt.t
   val toNanoseconds: t -> LargeInt.t
   val toReal: t -> LargeReal.t
   val toSeconds: t -> LargeInt.t
   val toString: t -> string
   val zero: t

end
