signature STRING = sig

   include MONO_VECTOR where type 'a elem = char

   val hasPrefix: t * t -> bool
   val toLower: t -> t
   val toUpper: t -> t

end
