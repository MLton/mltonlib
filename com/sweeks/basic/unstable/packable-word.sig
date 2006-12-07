signature PACKABLE_WORD = sig

   include WORD

   val subArr: Word8.t array * int * Endian.t -> t
   val subVec: Word8.t vector * int * Endian.t -> t
   val update: Word8.t array * int * t * Endian.t -> unit
      
end
