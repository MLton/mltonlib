exception NoSolution

functor GCD(E : EUCLIDEAN_DOMAIN) =
  struct
    local
      structure B = EuclideanDomainPercent(E)
      open B
    in
      (* (g, i, j, x)  = egcd(a, b)
       * gcd(a,b) = g = a*i + b*j
       * x = -1 ->  i <= 0 and j >= 0
       * x = +1 ->  j <= 0 and i >= 0
       *)
      fun egcd (a, b) =
        if (b =% #%0) then (a, #%1, #%0, #%1) else
        let
          val (q, r)       = a //% b
          val (g, i, j, x) = egcd (b, r)
          in
          (g, j, i -% q*%j, ~%x)
        end
      
      fun gcd (a, b) = case egcd (a, b) of (g, _, _, _) => g
      fun lcm (a, b) = a*%b /% gcd(a, b)
      
      fun gcdinv (n, m) =
        let
          val (g, ni, mi, x) = egcd(n, m)
        in
          if x =% #%1 
          then (g, ni, n+%mi)
          else (g, m+%ni, mi)
        end
    
    fun inv (a, n) = case gcdinv (n, a) of (_, _, ai) => ai
    
    fun crt nil = (#%0, #%1)
      | crt ((a, n) :: x) =
        let
          val (b, m) = crt x
          val (g, ni, mi) = gcdinv (n, m)
          val l = n*%m/%g
        in
          if a %% g =% b %% g
          then ((a*%m*%mi +% b*%n*%ni)/%g %% l, l)
          else raise NoSolution
        end
    end
  end
