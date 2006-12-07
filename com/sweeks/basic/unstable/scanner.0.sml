structure Scanner = struct

   type 'a t = char seq -> ('a * char seq) option

   val make = id

   fun scan (s, cs) = s cs

end
