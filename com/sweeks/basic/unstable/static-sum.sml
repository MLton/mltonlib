structure StaticSum:> STATIC_SUM = struct

   type ('a1, 'a2, 'b1, 'b2, 'c) u = ('a1 -> 'a2) * ('b1 -> 'b2) -> 'c
   type ('a1, 'a2, 'b1, 'b2, 'c) t = unit -> ('a1, 'a2, 'b1, 'b2, 'c) u

   fun left a1 = fn () => fn (f, _) => f a1
   fun right b1 = fn () => fn (_, f) => f b1
   fun switch (f, l, r) = f () (l, r)

end
