structure Scanner = struct

   datatype 'a t = T of Char.t Seq.t -> ('a * Char.t Seq.t) Option.t

   val make = T

   fun scan (T s, cs) = s cs

end
