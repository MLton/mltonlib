(****************************************************************** Algebra ops *)

functor ScalarMultiply(S : SCALAR_MULTIPLY) =
  struct
    val (op *&) = S.MUL
  end

functor Module(M : MODULE) =
  struct
    local structure S = RingPercent(M.Base) in open S end
    local structure S = AbelianGroupAddDollar(M.Addition) in open S end
    local structure S = ScalarMultiply(M.ScalarMultiplication) in open S end
  end

functor UnitaryModule(U : UNITARY_MODULE) =
  struct
    local structure S = UnitaryRingPercent(U.Base) in open S end
    local structure S = AbelianGroupAddDollar(U.Addition) in open S end
    local structure S = ScalarMultiply(U.ScalarMultiplication) in open S end
  end

functor VectorSpace(V : VECTOR_SPACE) =
  struct
    local structure S = FieldPercent(V.Base) in open S end
    local structure S = AbelianGroupAddDollar(V.Addition) in open S end
    local structure S = ScalarMultiply(V.ScalarMultiplication) in open S end
  end

functor Algebra(A : ALGEBRA) =
  struct
    local structure S = CommutativeRingPercent(A.Base) in open S end
    local structure S = NonAssociativeRingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end

functor FieldAlgebra(A : FIELD_ALGEBRA) =
  struct
    local structure S = FieldPercent(A.Base) in open S end
    local structure S = NonAssociativeRingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end

functor AssociativeAlgebra(A : ASSOCIATIVE_ALGEBRA) =
  struct
    local structure S = CommutativeRingPercent(A.Base) in open S end
    local structure S = RingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end

functor AssociativeFieldAlgebra(A : ASSOCIATIVE_FIELD_ALGEBRA) =
  struct
    local structure S = FieldPercent(A.Base) in open S end
    local structure S = RingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end

functor UnitaryAssociativeAlgebra(A : UNITARY_ASSOCIATIVE_ALGEBRA) =
  struct
    local structure S = CommutativeRingPercent(A.Base) in open S end
    local structure S = UnitaryRingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end

functor UnitaryAssociativeFieldAlgebra(A : UNITARY_ASSOCIATIVE_FIELD_ALGEBRA) =
  struct
    local structure S = FieldPercent(A.Base) in open S end
    local structure S = UnitaryRingDollar(A) in open S end
    local structure S = ScalarMultiply(A.ScalarMultiplication) in open S end
  end
