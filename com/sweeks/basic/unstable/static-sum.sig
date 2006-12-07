signature STATIC_SUM = sig
   
   type ('a1, 'a2, 'b1, 'b2, 'c) u
   type ('a1, 'a2, 'b1, 'b2, 'c) t = unit -> ('a1, 'a2, 'b1, 'b2, 'c) u

   val left: 'a1 -> ('a1, 'a2, 'b1, 'b2, 'a2) t
   val right: 'b1 -> ('a1, 'a2, 'b1, 'b2, 'b2) t
   val switch: ('a1, 'a2, 'b1, 'b2, 'c) t * ('a1 -> 'a2) * ('b1 -> 'b2) -> 'c

end
