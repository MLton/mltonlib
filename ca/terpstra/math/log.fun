(*
functor DiscreteLogarithm(G : GROUP) =
  struct
    local
      structure B = GroupMulPercent(G)
      structure D = GCD(Z)
      open B D Factor LargeInt
      
      exception NotDiscrete
      val order =
        case G.order of
          FINITE x => x
        | _ => raise NotDiscrete
        
      datatype Factorization =
          PRIME of LargeInt.int
        | POWER of Order * LargeInt.int
        | COPRIME of Order * Order
      withtype Order = LargeInt.int * Factorization
      
      val factors = factor order
      
      fun log (n, f)
    in
      fun log = crtLog
    end
  end
*)
