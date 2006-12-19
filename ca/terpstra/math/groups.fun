(****************************************************************** Group ops *)

exception ImpossibleConstant

functor SetPercent(S : BINARY_OPERATION) =
  struct
    val (op =% ) = S.EQ
    val (op !=%) = not o S.EQ
  end

functor SetDollar(S : BINARY_OPERATION) =
  struct
    val (op =$ ) = S.EQ
    val (op !=$) = not o S.EQ
  end

functor BinaryOperationMulPercent(B : BINARY_OPERATION) =
  struct
    local structure S = SetPercent(B) in open S end
    val (op *% )  = B.MUL
    local open LargeInt in
      fun _ **% 0 = raise ImpossibleConstant
        | x **% 1 = x
        | x **% e = 
          let val h = x **% (e div 2) 
          in if (e mod 2) = 0 then h *% h else h *% h *% x end
    end
  end
functor BinaryOperationAddPercent(B : BINARY_OPERATION) =
  struct
    local structure S = SetPercent(B) in open S end
    val (op +% )  = B.MUL
    local open LargeInt in
      fun _ ++% 0 = raise ImpossibleConstant
        | x ++% 1 = x
        | x ++% e = 
          let val h = x ++% (e div 2) 
          in if (e mod 2) = 0 then h +% h else h +% h +% x end
    end
  end

functor BinaryOperationMulDollar(B : BINARY_OPERATION) =
  struct
    local structure S = SetDollar(B) in open S end
    val (op *$ ) = B.MUL
    local open LargeInt in
      fun _ **$ 0 = raise ImpossibleConstant
        | x **$ 1 = x
        | x **$ e = 
          let val h = x **$ (e div 2) 
          in if (e mod 2) = 0 then h *$ h else h *$ h *$ x end
    end
  end
functor BinaryOperationAddDollar(B : BINARY_OPERATION) =
  struct
    local structure S = SetDollar(B) in open S end
    val (op +$ ) = B.MUL
    local open LargeInt in
      fun _ ++$ 0 = raise ImpossibleConstant
        | x ++$ 1 = x
        | x ++$ e = 
          let val h = x ++$ (e div 2) 
          in if (e mod 2) = 0 then h +$ h else h +$ h +$ x end
    end
  end

functor SemiGroupMulPercent(S : SEMIGROUP) = BinaryOperationMulPercent(S)
functor SemiGroupAddPercent(S : SEMIGROUP) = BinaryOperationAddPercent(S)
functor SemiGroupMulDollar (S : SEMIGROUP) = BinaryOperationMulDollar (S)
functor SemiGroupAddDollar (S : SEMIGROUP) = BinaryOperationAddDollar (S)

functor MonoidMulPercent(M : MONOID) = 
  struct
    local structure S = SemiGroupMulPercent(M) in open S end
    val (op **%) = fn (_, 0) => M.one | (x, e) => x **% e
    val #% = fn 1 => M.one | _ => raise ImpossibleConstant
  end
functor MonoidAddPercent(M : MONOID) = 
  struct
    local structure S = SemiGroupAddPercent(M) in open S end    
    val (op ++%) = fn (_, 0) => M.one | (x, e) => x ++% e
    val #% = fn 0 => M.one | _ => raise ImpossibleConstant
  end

functor MonoidMulDollar(M : MONOID) = 
  struct
    local structure S = SemiGroupMulDollar(M) in open S end
    val (op **$) = fn (_, 0) => M.one | (x, e) => x **$ e
    val #$ = fn 1 => M.one | _ => raise ImpossibleConstant
  end
functor MonoidAddDollar(M : MONOID) = 
  struct
    local structure S = SemiGroupAddDollar(M) in open S end
    val (op ++$) = fn (_, 0) => M.one | (x, e) => x ++$ e
    val #$ = fn 0 => M.one | _ => raise ImpossibleConstant
  end

functor AbelianMonoidMulPercent(A : ABELIAN_MONOID) = MonoidMulPercent(A)
functor AbelianMonoidAddPercent(A : ABELIAN_MONOID) = MonoidAddPercent(A)
functor AbelianMonoidMulDollar (A : ABELIAN_MONOID) = MonoidMulDollar (A)
functor AbelianMonoidAddDollar (A : ABELIAN_MONOID) = MonoidAddDollar (A)

functor GroupMulPercent(G : GROUP) =
  struct
    local structure S = MonoidMulPercent(G) in open S end
    val (op !% ) = G.INV
    val (op **%) = fn (x, e) => if e < 0 then !%x **% ~e else x **% e
  end
functor GroupAddPercent(G : GROUP) =
  struct
    local structure S = MonoidAddPercent(G) in open S end
    val (op ~% ) = G.INV
    val (op ++%) = fn (x, e) => if e < 0 then ~%x ++% ~e else x ++% e
  end

functor GroupMulDollar(G : GROUP) =
  struct
    local structure S = MonoidMulDollar(G) in open S end
    val (op !$ ) = G.INV
    val (op **$) = fn (x, e) => if e < 0 then !$x **$ ~e else x **$ e
  end
functor GroupAddDollar(G : GROUP) =
  struct
    local structure S = MonoidAddDollar(G) in open S end
    val (op ~$ ) = G.INV
    val (op ++$) = fn (x, e) => if e < 0 then ~$x ++$ ~e else x ++$ e
  end

functor AbelianGroupMulPercent(A : ABELIAN_GROUP) = 
  struct
    local structure S = GroupMulPercent(A) in open S end
    val (op /%) = A.DIV
  end
functor AbelianGroupAddPercent(A : ABELIAN_GROUP) = 
  struct
    local structure S = GroupAddPercent(A) in open S end
    val (op -%) = A.DIV
  end

functor AbelianGroupMulDollar(A : ABELIAN_GROUP) = 
  struct
    local structure S = GroupMulDollar(A) in open S end
    val (op /$) = A.DIV
  end
functor AbelianGroupAddDollar(A : ABELIAN_GROUP) = 
  struct
    local structure S = GroupAddDollar(A) in open S end
    val (op -$) = A.DIV
  end
