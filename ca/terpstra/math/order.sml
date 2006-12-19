structure Order : EUCLIDEAN_DOMAIN =
  struct
    local
      exception Undefined
      open LargeInt
    in
      type t = order
      val characteristic = LargeInt.fromInt 0
      
      structure Addition = 
        struct
          type t = order
          val order = UNCOUNTABLE
          
          val associative = ()
          val commutative = ()
          val one = FINITE 0
          
          val EQ = (op =)
          fun MUL (FINITE x, FINITE y) = FINITE (x+y)
            | MUL (UNCOUNTABLE, _) = UNCOUNTABLE
            | MUL (_, UNCOUNTABLE) = UNCOUNTABLE
            | MUL _ = COUNTABLE
          fun INV (FINITE x) = FINITE (~x)
            | INV _ = raise Undefined
          fun DIV (x, y) = MUL (x, INV y)
        end
      
      structure Multiplication = 
        struct
          type t = order
          val order = UNCOUNTABLE
          
          val associative = ()
          val commutative = ()
          val one = FINITE 1
          
          val EQ = (op =)
          fun MUL (FINITE x, FINITE y) = FINITE (x*y)
            | MUL (_, FINITE 0) = raise Undefined
            | MUL (FINITE 0, _) = raise Undefined
            | MUL (UNCOUNTABLE, _) = UNCOUNTABLE
            | MUL (_, UNCOUNTABLE) = UNCOUNTABLE
            | MUL _ = COUNTABLE
        end
      
      val distributive = ()
      val no_zero_divisors = ()
      
      fun QUO (FINITE a, FINITE b) = (FINITE (a div b), FINITE (a mod b))
        | QUO _ = raise Undefined
      
      fun LT (UNCOUNTABLE, _) = false
        | LT (_, UNCOUNTABLE) = true
        | LT (COUNTABLE, _)   = false
        | LT (_, COUNTABLE)   = true
        | LT (FINITE x, FINITE y) = x < y
    end
  end
