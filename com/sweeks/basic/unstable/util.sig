signature UTIL = sig

   val const: 'a -> 'b -> 'a
(*   val cross: ('a -> 'b) * ('c -> 'd) -> 'a * 'c -> 'b * 'd *)
(*   val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c*)
   val die: String.t -> 'a
   val fst: 'a * 'b -> 'a
   val id: 'a -> 'a
   val o: ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
   val recur: 'a * ('a * ('a -> 'b) -> 'b) -> 'b
   val snd: 'a * 'b -> 'b

end
              
