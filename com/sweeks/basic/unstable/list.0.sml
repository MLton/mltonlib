structure List = struct

   datatype t = datatype list
      
   fun fold (l, b, f) = let
      fun loop (l, b) =
         case l of
            [] => b
          | x :: l => loop (l, f (x, b))
   in
      loop (l, b)
   end

   fun reverse l = fold (l, [], op ::)

   fun toSeq l = #1 (Seq.unfold (l, fn [] => None | x :: l => Some (x, l)))
      
end
