functor EuclideanDomainOfInteger(Integer : INTEGER) : EUCLIDEAN_DOMAIN =
  struct
    type t = Integer.int
    val characteristic = LargeInt.fromInt 0
    
    structure Addition =
      struct
        type t = Integer.int
        val order = COUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Integer.fromInt 0
        
        val EQ  = (op =)
        val MUL = Integer.+
        val DIV = Integer.-
        val INV = Integer.~
      end
    
    structure Multiplication =
      struct
        type t = Integer.int
        val order = COUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Integer.fromInt 1
        
        val EQ  = (op =)
        val MUL = Integer.*
      end
    
    val distributive     = ()
    val no_zero_divisors = ()
    
    fun QUO (a, b) = (Integer.div (a, b), Integer.mod (a, b))
    val LT = Integer.<
  end

functor EuclideanDomainOfWord(Word : WORD) : EUCLIDEAN_DOMAIN =
  struct
    type t = Word.word
    val characteristic = LargeInt.fromInt 0
    
    structure Addition =
      struct
        type t = Word.word
        val order = COUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Word.fromInt 0
        
        val EQ  = (op =)
        val MUL = Word.+
        val DIV = Word.-
        val INV = Word.~
      end
    
    structure Multiplication =
      struct
        type t = Word.word
        val order = COUNTABLE
        
        val associative = ()
        val commutative = ()
        val one = Word.fromInt 1
        
        val EQ  = (op =)
        val MUL = Word.*
      end
    
    val distributive     = ()
    val no_zero_divisors = ()
    
    fun QUO (a, b) = (Word.div (a, b), Word.mod (a, b))
    val LT = Word.<
  end
