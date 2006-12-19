signature GaloisParam =
  sig
    structure W : WORD
    val base : W.word
  end

functor GaloisFromTable(P : GaloisParam) : FIELD =
  struct
    local
      open P.W
      open Int
      
      val fromInt = P.W.fromInt
      val toInt   = P.W.toInt
      
      val zero = fromInt 0
      val one  = fromInt 1
      
      val msize = toInt (notb zero)
      val size = msize + 1
      val highbit = << (one, Word.-(Word.fromInt wordSize, 0w1))
      
      fun nastymul x y =
        if y = zero then zero else
        if y = one then x else
        let val h = nastymul x (>> (y, 0w1))
            val x = if andb (y, one) = one then x else zero
            val b = if andb (h, highbit) = highbit then P.base else zero
        in xorb( << (h, 0w1), xorb(x, b)) end
      
      val gen = orb (one, << (one, 0w1)) (* x^1 + x^0 <- a generator *)
      
      val log = Array.array (size, zero)
      fun set _ l ~1 = l
        | set a l i = (
            Array.update (log, toInt a, fromInt i)
            ; set (nastymul a gen) (a :: l) (i - 1))
      
      val exp = Vector.fromList (set gen nil (msize + msize - 1))
      val log = Array.vector log
(*      
      val _ = Vector.app (fn x => print (P.W.toString x ^ " ")) exp
      val _ = print "\n"
      val _ = Vector.app (fn x => print (P.W.toString x ^ " ")) log
      val _ = print "\n"
*)      
      val exp = fn x => Vector.sub (exp, x)
      val log = fn x => toInt (Vector.sub (log, toInt x))
    in
      type t = word
      val characteristic = LargeInt.fromInt 2
      
      structure Addition =
        struct
          type t = word
          val order = FINITE (Int.toLarge msize)
          
          val associative = ()
          val commutative = ()
          val one = fromInt 0
          
          val EQ  = (op =)
          val MUL = fn (x, y) => xorb (x, y)
          val DIV = MUL
          val INV = fn x => x
        end
      
      structure Multiplication =
        struct
          type t = word
          val order = FINITE (Int.toLarge size)
          
          val associative = ()
          val commutative = ()
          val one = fromInt 1
          
          val EQ  = (op =)
          val MUL = fn (x, y) =>
            if x = zero orelse y = zero then zero else
            let val (lx, ly) = (log x, log y)
            in exp (lx + ly) end
          val DIV = fn (x, y) =>
            let val (lx, ly) = (log x, log y)
            in exp (lx + msize - ly) end
          val INV = fn x => 
            let val lx = log x
            in exp (msize - lx) end
        end
      
      val distributive = ()
      val no_zero_divisors = ()
    end
 end
