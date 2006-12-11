signature STRING = sig

   include MONO_VECTOR where type 'a elem = char

   val hasPrefix: t * t -> bool
   val ofWord8Vector: Word8.t vector -> t
   val toLower: t -> t
   val toUpper: t -> t
   val toWord8Vector: t -> Word8.t vector

end
