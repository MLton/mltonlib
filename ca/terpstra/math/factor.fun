functor Factor(E : EUCLIDEAN_DOMAIN) =
  struct
    local
      structure B = EuclideanDomainPercent(E)
      open B
      
      fun push (z, l) =
        case l of
          nil => (z, #%1) :: nil
        | (y, e) :: l => 
            if y =% z 
            then (y, e +% #%1) :: l
            else (z, #%1) :: (y, e) :: l
          
      fun niaveFactor (z, l) a =
        if z <% a*%a then push (z, l) else
        if z %% a =% #%0 then niaveFactor (z /% a, push (a, l)) a else
        niaveFactor (z, l) (a +% #%1)
    in
      fun factor z = niaveFactor (z, nil) (#%2)
    end
  end
