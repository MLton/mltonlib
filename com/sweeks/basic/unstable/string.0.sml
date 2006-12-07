structure String = struct

   open String
      
   fun toSeq s =
      #1 (Seq.unfold (0, fn i =>
                      if i = size s then
                         None
                      else
                         Some (String.sub (s, i), i + 1)))

end
