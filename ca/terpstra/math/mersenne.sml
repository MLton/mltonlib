(* all Mersenne primes up to 64 bits: 2, 3, 5, 7, 13, 17, 19, 31, 61 *)
(* In order to use 61 we need a Word128 on 64bit machines *)

local
  structure P7 =
    struct
      structure Z = Word8
      structure ZZ = Word16
      val bits = 0w7
    end
  structure P13 =
    struct
      structure Z = Word16
      structure ZZ = Word32
      val bits = 0w13
    end
  structure P31 =
    struct
      structure Z = Word32
      structure ZZ = Word64
      val bits = 0w31
    end
in
  structure Mersenne7  = Mersenne(P7)
  structure Mersenne13 = Mersenne(P13)
  structure Mersenne31 = Mersenne(P31)
end
