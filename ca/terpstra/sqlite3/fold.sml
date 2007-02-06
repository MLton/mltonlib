(* Shamelessly stolen from Vesa *)

fun $ (a, f) = f a
structure Fold =
  struct
    fun fold (a, f) g = g (a, f)
    fun step0 h (a, f) = fold (h a, f)
    fun step1 h (a, f) b = fold (h (b, a), f)
  end

structure Foldr =
  struct
    fun foldr (a, f) = Fold.fold (f, fn g => g a)
    fun step0 h = Fold.step0 (fn g => g o h)
    fun step1 h = Fold.step1 (fn (b, g) => g o (fn a => h (b, a)))
  end
