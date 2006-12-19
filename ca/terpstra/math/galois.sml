local
  structure P4 = 
    struct 
      structure W = Word4
      val base : W.word = 0wx3 
    end
  structure P5 =
    struct
      structure W = Word5
      val base : W.word = 0wx9
    end
  structure P8 = 
    struct 
      structure W = Word8 
      val base : W.word = 0wx1B
    end
in
  structure Galois4 = GaloisFromTable(P4)
  structure Galois5 = GaloisFromTable(P5)
  structure Galois8 = GaloisFromTable(P8)
end
