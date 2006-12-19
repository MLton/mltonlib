functor FieldOfReal(Real : REAL) : FIELD =
  struct
    type t = Real.real
    val characteristic = LargeInt.fromInt 0
    
    structure Addition = 
      struct
        type t = Real.real
        val order = UNCOUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Real.fromInt 0
        
        val EQ  = Real.==
        val MUL = Real.+
        val DIV = Real.-
        val INV = Real.~
      end
    
    structure Multiplication =
      struct
        type t = Real.real
        val order = UNCOUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Real.fromInt 1
        
        val EQ  = Real.==
        val MUL = Real.*
        val DIV = Real./
        val INV = fn x => DIV (one, x)
      end
    
    val distributive     = ()
    val no_zero_divisors = ()
  end
