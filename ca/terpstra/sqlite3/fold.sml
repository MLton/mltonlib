(* Shamelessly stolen from Vesa *)

fun $ (a, f) = f a
structure Fold =
   struct
      type ('a, 'b, 'c, 'd) step = 'a * ('b -> 'c) -> 'd
      type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd) step -> 'd
      type ('a1, 'a2, 'b, 'c, 'd) step0 = ('a1, 'b, 'c, ('a2, 'b, 'c, 'd, unit) t) step
      type ('a11, 'a12, 'a2, 'b, 'c, 'd) step1 = ('a12, 'b, 'c, 'a11 -> ('a2, 'b, 'c, 'd, unit) t) step
      
      fun fold (a, f) g = g (a, f)
      fun step0 h (a, f) = fold (h a, f)
      fun step1 h (a, f) b = fold (h (b, a), f)
   end

structure Foldr : FOLD =
   struct
      (* Need help cleaning up this disaster *)
      type ('a, 'b, 'c, 'd, 'e) t = (('b -> 'c) * (('a -> 'd) -> 'd) -> 'e) -> 'e
      type ('a1, 'a2, 'b, 'c, 'd) step0 = ('a2 -> 'b) * 'c -> (('a1  -> 'b) * 'c -> 'd) -> 'd
      type ('a11, 'a12, 'a2, 'b, 'c, 'd) step1 = ('a2 -> 'b) * 'c -> 'a11 -> (('a12 -> 'b) * 'c -> 'd) -> 'd
      
      fun fold (a, f) = Fold.fold (f, fn g => g a)
      fun step0 h = Fold.step0 (fn g => g o h)
      fun step1 h = Fold.step1 (fn (b, g) => g o (fn a => h (b, a)))
   end
