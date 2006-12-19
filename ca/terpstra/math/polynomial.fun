signature POLYNOMIAL =
  sig 
    include EUCLIDEAN_DOMAIN
    (* include UNITARY_ASSOCIATIVE_FIELD_ALGEBRA: *)
    type e 
    structure Base : FIELD where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
    
    val fromList: e list -> t
    val eval: t -> e -> e
    val primitive: t -> bool
  end

functor PolynomialOverField(F : FIELD) : POLYNOMIAL =
  struct
    local
      structure B = FieldPercent(F)
      structure V = Vector
      open B
      open V
      open VectorSlice
      
      val zero = #%0
      val one = #%1
      
      fun constant c = full (tabulate (1, fn _ => c))
      fun trim p n = subslice (p, 0, if n < length p then SOME n else NONE)
      fun smul (x, y) = map (fn a => x *% a) y
      
      (* reverse the polnomial's coeffs; p(1/x)*x^deg(p) *)
      fun rev p = 
        let
          val b = length p - 1
        in
          full (tabulate (length p, fn i => sub (p, b - i)))
        end
      
      fun normalize p =
        let
          val (v, s, l) = base p
          fun lastzero i =
            if i = s then 0 else
            let 
              val i1 = i - 1 
            in
              if V.sub (v, i1) =% zero then lastzero i1 else i-s
            end
        in
          subslice (p, 0, SOME (lastzero (s+l)))
        end
      
      fun padd (x, y) =
        let
          val (v1, v2) = 
            if length x < length y then (x, y) else (y, x)
          val len1 = length v1 (* shorter length *)
          val len2 = length v2
          fun add i =
            if i < len1
            then sub (v1, i) +% sub (v2, i)
            else sub (v2, i)
        in
          full (tabulate (len2, add))
        end
      
      fun psub (x, y) =
        let
          val tab =
            if length x < length y
            then (length y,
              let 
                val len = length x
              in
                fn i =>
                  if i < len
                  then sub (x, i) -% sub (y, i)
                  else ~% (sub (y, i))
              end)
            else (length x,
              let
                val len = length y
              in
                fn i =>
                  if i < len
                  then sub (x, i) -% sub (y, i)
                  else sub (x, i)
              end)
        in
          full (tabulate tab)
        end
      
      (* long multiplication *)
      fun long (x, y) = (* length x < length y *)
        if length x = 1 then smul (sub (x, 0), y) else
        let
          val l = smul (sub (x, 0), y)
          val hx = subslice (x, 1, NONE)
          val h = long (hx, y)
          fun join i = 
            if i = 0 then V.sub (l, 0) else
            if i >= V.length l then V.sub (h, i-1) else
            V.sub (l, i) +% V.sub (h, i-1)
        in
          tabulate (length x + length y - 1, join)
        end
        
      (* karatsuba's trick *)
      fun kara (x, y) = (* length x < length y *)
        let val (lx, ly) = (length x, length y) (* example: 5, 8 *) in
        if lx < 5 then long (x, y) else
        (* (a*x+b)*(c*x+d) = a*c*x + (a*d+b*c)*x + b*d 
         * (a+b)*(c+d) = (a*c + (a*d + b*c) + b*d)
         *)
        let
          (* lx2 <= rx <= ry *)
          val lx2 = lx div 2                   (* example: 2 *)
          val (rx, ry) = (lx - lx2, ly - lx2)  (* example: 3, 6 *)
          val lxy = lx + ly - 1                (* example: 12 *)
          val lxh = lx2 + lx2                  (* example: 4 *)
          val lxf = lxh - 1                    (* example: 3 *)
          val rxy = rx + ry - 1                (* example: 8 *)
          val lx2r = lx2 + rxy                 (* example: 10 *)
          
          val a = subslice (x, lx2, NONE)      (* length = rx  (3) *)
          val b = subslice (x, 0, SOME lx2)    (* length = lx2 (2) *)
          val c = subslice (y, lx2, NONE)      (* length = ry  (6) *)
          val d = subslice (y, 0, SOME lx2)    (* length = lx2 (2) *)
          
          val ab = padd (a, b)                 (* length = rx  (3) *)
          val cd = padd (c, d)                 (* length = ry  (6) *)
          val abcd = kara (ab, cd)             (* length = rxy (8) *)
          val ac = kara (a, c)                 (* length = rxy (8) *)
          val bd = kara (b, d)                 (* length = lxf (3) *)
          
          val (adbc, _, _) = 
            base (psub(
              full abcd, padd(full ac, full bd)))
          
          (*         v-- lxh
           *  ---ac---
           *   ^--adbc--
           *   |      ^bd-
           *   |      |^ 0
           *   |      |lx2
           *   lx2r   lxf
           *)
          (*!!! all these tests are grossly inefficient *)
          fun join i =
            case Int.compare (i, lxf) of
            LESS => if i < lx2 
                then V.sub (bd, i)
                else V.sub (adbc, i-lx2) +% V.sub (bd, i)
            | EQUAL => V.sub (adbc, i-lx2)
            | GREATER =>
                if i < lx2r 
                then V.sub (ac, i-lxh) +% V.sub (adbc, i-lx2)
                else V.sub (ac, i-lxh)
        in
          V.tabulate (lxy, join)
        end end
      
      (* !!! No FFT yet *)
      fun pmul (x, y) = 
        if length x < length y
        then if length x = 0 then x else full (kara (x, y))
        else if length y = 0 then y else full (kara (y, x))
      
      (* !!! write an inner product method *)
      
      (* !!! don't be stupid; actually use the shortcut *)
      fun pmuls (x, y, l) =
        let
          val xs = trim x l
          val ys = trim y l
          val z = pmul (xs, ys)
        in
          trim z l
        end
      
      (* p(x)*q(x) = 1 + r(x)*x^2^n
       * => 2-p(x)*q(x) = 1 - r(x)*x^2^n
       * => (2-p(x)*q(x))*p(x)*q(x) = 1 - r(x)^2*x^2^(n+1)
       * => q' = (2-pq)q
       *)
      fun invmodn p n = 
        let
          val init = constant (!% (sub (p, 0)))
          val two  = constant (#%2)
          fun grow q =
            if length q >= n then q else
            let
              val nl = length q + length q
              val ps = trim p nl
              val pq = pmul (q, ps) (* !!! we don't need high terms... *)
              val neg = psub (two, pq)
              (* !!! use inner product; we know low terms and don't need high *)
              val nq = pmul (neg, q)
            in
              if length p = 1 then init else grow (trim nq nl)
            end
        in
          trim (grow init) n
        end
    in
      type e = F.t
      type t = e slice
      val characteristic = F.characteristic
      
      fun eval p x = foldr (fn (a, out) => out *% x +% a) zero p
      val fromList = normalize o full o fromList
      fun primitive p = sub (p, length p - 1) =% one
      
      structure Base = F
      
      structure Addition =
        struct
          type t = t
          val order = case Base.Addition.order of 
            UNCOUNTABLE => UNCOUNTABLE
            | _ => COUNTABLE
          
          val associative = ()
          val commutative = ()
          val one = full (tabulate (0, fn _ => #%0))
          
          val EQ = fn (x, y) =>
            let
              val (v1, s1, l1) = base x
              val (v2, s2, l2) = base y
              val e1 = l1 + s1
              fun compare (i1, i2) =
                if i1 = e1 then true else
                if V.sub (v1, i1) =% V.sub (v2, i2)
                then compare (i1 + 1, i2 + 1)
                else false
            in
              if l1 <> l2 then false else
              compare (s1, s2)
            end
          
          val MUL = normalize o padd
          val DIV = normalize o psub
          val INV = fn x => full (map (fn a => ~%a) x)
        end
      
      structure Multiplication = 
        struct
          type t = t
          val order = Addition.order
          
          val associative = ()
          val commutative = ()
          val one = full (tabulate (1, fn _ => #%1))
          
          val EQ = Addition.EQ
          val MUL = normalize o pmul
        end
      
      structure ScalarMultiplication =
        struct
          type e = e
          type t = t
          
          val associative = ()
          val distributive = ()
          
          val MUL = normalize o full o smul
        end
      
      val QUO = fn (z, y) =>
        let
          (* q*y + r = z, where deg(r) < deg(y) and deg(q)+deg(y)=deg(z)
           * => rev(q*y + r) = rev(z)
           * => rev(q)rev(y) + rev(r)*x^(deg(z)-deg(r)) = rev(z)
           * => rev(q)rev(y) = rev(z) mod x^(1+deg(z)-deg(y))
           * => rev(q) = rev(z)/rev(y) mod x^(1+deg(q))
           *)
          val degz = length z - 1
          val degy = length y - 1
        in
          if degz < degy then (Addition.one, z) else
          let
            val degq = degz - degy
            val lenq = degq + 1
            val ry = rev y
            val iry = invmodn ry lenq
            val rz = rev z
            val rq = pmuls (rz, iry, lenq)
            val q = rev rq
            val qy = pmuls (q, y, degy)
            val r = psub (trim z degy, qy)
          in
            (q, normalize r)
          end
        end
      
      fun LT (x, y) = length x < length y
      
      val distributive = ()
      val no_zero_divisors = ()
      val bilinear = ()
    end
  end

functor Polynomial(P : POLYNOMIAL) =
  struct
    local structure S = FieldPercent(P.Base) in open S end
    local structure S = EuclideanDomainDollar(P) in open S end
    local structure S = ScalarMultiply(P.ScalarMultiplication) in open S end
  end
