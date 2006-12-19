signature MERSENNE_BASE =
  sig
    structure Z  : WORD (* fits the numbers with 1 bit to spare *)
    structure ZZ : WORD (* fits the product *)
    val bits : word
  end

exception MersenneOverflow

functor Mersenne(M : MERSENNE_BASE) : FIELD =
  struct
    local
      structure Z = M.Z
      structure ZZ = M.ZZ
      structure O = EuclideanDomainDollar(Order)
      structure G = GCD(EuclideanDomainOfWord(Z))
      open Z
      open O
      open G
      
      val mbits = M.bits
      val mmbits = Word.+ (mbits, mbits)
      val rbits = Word.fromInt wordSize
      val rrbits = Word.fromInt ZZ.wordSize
      val rbitsm1 = Word.- (rbits, 0w1)
      
      val _ = if Word.>= ( mbits,  rbits) then raise MersenneOverflow else ()
      val _ = if Word.>= (mmbits, rrbits) then raise MersenneOverflow else ()
      
      val mmask = << (fromInt 1, mbits) - fromInt 1
    in
      type t = word
      val characteristic = toLargeInt mmask
      
      structure Addition =
        struct
          type t = word
          val order = FINITE characteristic
          
          val associative = ()
          val commutative = ()
          val one = fromInt 0
          
          val EQ  = (op =)
          val MUL = fn (x, y) => 
            let val z = x + y in andb (z, mmask) + >> (z, mbits) end
          val DIV = fn (x, y) =>
            let val z = x - y in andb (z, mmask) - >> (z, rbitsm1) end
          val INV = fn x => mmask - x
        end
      
      structure Multiplication =
        struct
          type t = word
          val order = Addition.order -$ #$1
          
          val associative = ()
          val commutative = ()
          val one = fromInt 1
          
          val EQ  = (op =)
          val MUL = fn (x, y) =>
            let
              val ZZ = ZZ.fromLarge o toLarge
              val Z = fromLarge o ZZ.toLarge
              val z = ZZ.* (ZZ x, ZZ y)
            in
              Addition.MUL (Z (ZZ.>> (z, mbits)), andb (Z z, mmask))
            end
          val INV = fn x => inv (x, mmask)
          val DIV = fn (x, y) => MUL (x, INV y)
        end
      
      val distributive = ()
      val no_zero_divisors = ()
    end
 end
