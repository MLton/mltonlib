functor Enumerable (S: ENUMERATE): ENUMERABLE = struct

   open S

   fun recur (s, b, done, step) = let
      val (c, s) = start s
   in
      Util.recur
      ((s, b), fn ((s, b), loop) =>
       case next (c, s) of
          None => done b
        | Some (x, s) => step (x, b, fn b => loop (s, b)))
   end

   local
      structure S = Recur (open S
                           val recur = recur)
   in
      open S
   end
      
   fun toSeq e = let
      val (c, s) = start e
   in
      Seq.unfold (s, fn s => next (c, s))
   end

end
