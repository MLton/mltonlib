(* This is still a field only if (x^2 + 1) is irreducible *)
functor ComplexOfField(F : FIELD) : FIELD =
  struct
    local
      structure B = FieldPercent(F)
      structure O = EuclideanDomainDollar(Order)
      open B
      open O
    in
      type t = F.t * F.t
      val characteristic = F.characteristic
      
      structure Addition =
        struct
          type t = F.t * F.t
          val order = F.Addition.order *$ F.Addition.order
          
          val associative = ()
          val commutative = ()
          val one = (#%0, #%0)
          
          val EQ  = fn ((ar, ai), (br, bi)) => (ar =% br) andalso (ai =% bi)
          val MUL = fn ((ar, ai), (br, bi)) => (ar +% br, ai +% bi)
          val DIV = fn ((ar, ai), (br, bi)) => (ar -% br, ai -% bi)
          val INV = fn (a, b) => (~%a, ~%b)
        end
        
      structure Multiplication = 
        struct
          type t = F.t * F.t
          val order = F.Addition.order *$ F.Addition.order -$ #$1
          
          val associative = ()
          val commutative = ()
          val one = (#%1, #%0)
          
          val EQ  = fn ((ar, ai), (br, bi)) => (ar =% br) andalso (ai =% bi)
          val INV = fn (r, i) => 
            let val e = !%(r*%r +% i*%i) in (r*%e, ~%i*%e) end
          val MUL = fn ((ar, ai), (br, bi)) => (ar*%br -% ai*%bi, ar*%bi +% ai*%br)
          val DIV = fn (a, c) => MUL (a, INV c)
        end
      
      val distributive     = ()
      val no_zero_divisors = ()
    end
  end
