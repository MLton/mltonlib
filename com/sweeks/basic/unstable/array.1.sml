structure Array: ARRAY = struct

   open Array

   local
      structure S = RamSequence (type 'a t = 'a t
                                 val fromArray = id
                                 val size = size
                                 val sub = sub)
   in
      open S
   end

   fun updates (a, i, s) =
      ignore (Seq.fold (s, i, fn (x, i) => (update (a, i, x); i + 1)))

end


