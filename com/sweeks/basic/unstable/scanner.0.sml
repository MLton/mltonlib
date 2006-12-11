structure Scanner = struct

   type 'a t = Char.t Seq.t -> ('a * Char.t Seq.t) Option.t

   val make = id

   fun scan (s, cs) = s cs

end
