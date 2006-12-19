functor QuotientOfIntegralDomain(I : INTEGRAL_DOMAIN) : FIELD =
  struct
    local
      structure B = IntegralDomainPercent(I)
      structure O = EuclideanDomainDollar(Order)
      open B
      open O
    in
      type t = I.t * I.t
      val characteristic = I.characteristic
      
      structure Addition =
        struct
          type t = I.t * I.t
          val order = COUNTABLE (* !!! no idea how to compute ... *)
          
          val associative = ()
          val commutative = ()
          val one = (#%0, #%1)
          
          val MUL = fn ((an, ad), (bn, bd)) => 
            (an *% bd +% bn *% ad, ad *% bd)
          val DIV = fn ((an, ad), (bn, bd)) =>
            (an *% bd -% bn *% ad, ad *% bd)
          val INV = fn (an, ad) => (~%an, ad)
          
          val EQ  = fn ((an, ad), (bn, bd)) => 
            an *% bd =% bn *% ad
        end
        
      structure Multiplication =
        struct
          type t = I.t * I.t
          val order = COUNTABLE (* !!! no idea *)
          
          val associative = ()
          val commutative = ()
          val one = (#%1, #%1)
          
          val EQ = Addition.EQ
          val MUL = fn ((an, ad), (bn, bd)) =>
            (an *% bn, ad *% bd)
          val INV = fn (an, ad) => (ad, an)
          val DIV = fn (a, b) => MUL (a, INV b)
        end
      
      val distributive = ()
      val no_zero_divisors = ()
    end
  end

functor QuotientOfEuclideanDomain(E : EUCLIDEAN_DOMAIN) : FIELD =
  struct
    local
      structure B = EuclideanDomainPercent(E)
      structure G = GCD(E)
      structure Q = QuotientOfIntegralDomain(E)
      open B
      open G
    in
      type t = E.t * E.t
      val characteristic = E.characteristic
      
      fun simplify (n, d) =
        if d =% #%0 then Q.Addition.one else 
        let
          val g = gcd (n, d);
        in
          (n /% g, d /% g)
        end
      
      structure Addition =
        struct
          open Q.Addition
          
          (* easier to compute *)
          val EQ  = fn ((an, ad), (bn, bd)) => 
            ((an =%   bn) andalso (ad =%   bd)) orelse
            ((an =% ~%bn) andalso (ad =% ~%bd))
            
          val MUL = simplify o MUL
          val DIV = simplify o DIV
        end
        
      structure Multiplication =
        struct
          open Q.Multiplication
          
          val EQ = Addition.EQ
          val MUL = simplify o MUL
          val DIV = simplify o DIV
        end
      
      val distributive = ()
      val no_zero_divisors = ()
    end
  end
