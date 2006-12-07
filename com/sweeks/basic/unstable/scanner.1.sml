structure Scanner = struct

   open Scanner

   fun map (s, f) cs = Option.map (s cs, fn (x, cs) => (f x, cs))

   fun scanString (s, str) =
      case scan (s, String.toSeq str) of
         None => None
       | Some (x, s) =>
            if Seq.isEmpty s then
               Some x
            else
               None

   fun ofBasis b s = Option.ofBasis (b (Option.toBasis o Seq.get) s)

end
      
