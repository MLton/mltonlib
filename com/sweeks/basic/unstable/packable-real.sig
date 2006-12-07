signature PACKABLE_REAL = sig

   include REAL

   val ofBytes: Word8.t vector * Endian.t -> t
   val subArr: Word8.t array * int * Endian.t -> t
   val subVec: Word8.t vector * int * Endian.t -> t
   val toBytes: t * Endian.t -> Word8.t vector
   val update: Word8.t array * int * t * Endian.t -> unit

end
