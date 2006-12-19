(****************************************************************** Ring ops *)

functor NonAssociativeRingPercent(R : NON_ASSOCIATIVE_RING) =
  struct
    local structure S = BinaryOperationMulPercent(R.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(R.Addition) in open S end
  end
functor NonAssociativeRingDollar(R : NON_ASSOCIATIVE_RING) =
  struct
    local structure S = BinaryOperationMulDollar(R.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(R.Addition) in open S end
  end

functor RingPercent(R : RING) =
  struct
    local structure S = SemiGroupMulPercent(R.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(R.Addition) in open S end
  end
functor RingDollar(R : RING) =
  struct
    local structure S = SemiGroupMulDollar(R.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(R.Addition) in open S end
  end

functor UnitaryRingPercent(U : UNITARY_RING) =
  struct
    local structure S = MonoidMulPercent(U.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(U.Addition) in open S end
    val #% = fn e => U.Multiplication.one ++% e
  end
functor UnitaryRingDollar(U : UNITARY_RING) =
  struct
    local structure S = MonoidMulDollar(U.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(U.Addition) in open S end
    val #$ = fn e => U.Multiplication.one ++$ e
  end

functor CommutativeRingPercent(C : COMMUTATIVE_RING) = 
  struct
    local structure S = AbelianMonoidMulPercent(C.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(C.Addition) in open S end
    val #% = fn e => C.Multiplication.one ++% e
  end
functor CommutativeRingDollar(C : COMMUTATIVE_RING) =
  struct
    local structure S = AbelianMonoidMulDollar(C.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(C.Addition) in open S end
    val #$ = fn e => C.Multiplication.one ++$ e
  end

functor IntegralDomainPercent(I : INTEGRAL_DOMAIN) = CommutativeRingPercent(I)
functor IntegralDomainDollar (I : INTEGRAL_DOMAIN) = CommutativeRingDollar (I)

functor EuclideanDomainPercent(E : EUCLIDEAN_DOMAIN) =
  struct
    local structure S = AbelianMonoidMulPercent(E.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(E.Addition) in open S end
    val #% = fn e => E.Multiplication.one ++% e
    val (op /%) = #1 o E.QUO
    val (op %%) = #2 o E.QUO
    val (op //%) = E.QUO
    val (op <%) = E.LT
  end
functor EuclideanDomainDollar(E : EUCLIDEAN_DOMAIN) =
  struct
    local structure S = AbelianMonoidMulDollar(E.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(E.Addition) in open S end
    val #$ = fn e => E.Multiplication.one ++$ e
    val (op /$) = #1 o E.QUO
    val (op %$) = #2 o E.QUO
    val (op //$) = E.QUO
    val (op <$) = E.LT
  end

functor FieldPercent(F : FIELD) =
  struct
    local structure S = AbelianGroupMulPercent(F.Multiplication) in open S end
    local structure S = AbelianGroupAddPercent(F.Addition) in open S end
    val #% = fn e => F.Multiplication.one ++% e
  end
functor FieldDollar(F : FIELD) =
  struct
    local structure S = AbelianGroupMulDollar(F.Multiplication) in open S end
    local structure S = AbelianGroupAddDollar(F.Addition) in open S end
    val #$ = fn e => F.Multiplication.one ++$ e
  end
