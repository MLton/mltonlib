structure Option = struct

   open Option

   val toSeq =
      fn None => Seq.empty ()
       | Some x => Seq.single x

end

