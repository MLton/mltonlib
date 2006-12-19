(****************************************************************** Internal *)

signature NON_ASSOCIATIVE_RING_ =
  sig
    type t
    (* a*(b+c) = a*b + a*c
     * (b+c)*a = b*a + c*a
     *)
    val distributive : unit
    
    (* x++c = zero  [c=zero if no positive integer works] *)
    val characteristic : LargeInt.int
  end

signature RING_ =
  sig
    include NON_ASSOCIATIVE_RING_
  end

signature UNITARY_RING_ =
  sig
    include RING_
  end

signature COMMUTATIVE_RING_ =
  sig
    include UNITARY_RING_
  end

signature INTEGRAL_DOMAIN_ =
  sig
    include COMMUTATIVE_RING_
    (* a*b = 0 -> a = 0 or b = 0 *)
    val no_zero_divisors : unit
  end

signature EUCLIDEAN_DOMAIN_ =
  sig
    include INTEGRAL_DOMAIN_
    (* (a, b) -> (q, r) *)
    val QUO: (t * t) -> (t * t)
    (* There must exist a function, w (that need not be exposed) where:
     * b != 0 -> w(ab) >= w(a) 
     *           a = qb + r, r = 0 or w(r) < w(b)
     *)
    (* (a, b) -> w(a) < w(b) *)
    val LT: t * t -> bool
  end

signature FIELD_ =
  sig
    include INTEGRAL_DOMAIN_
  end

(****************************************************************** Rings *)

signature NON_ASSOCIATIVE_RING =
  sig
    include NON_ASSOCIATIVE_RING_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : BINARY_OPERATION where type t = t
  end

signature RING =
  sig
    include RING_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : SEMIGROUP where type t = t
  end

signature UNITARY_RING =
  sig
    include UNITARY_RING_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : MONOID where type t = t
  end

signature COMMUTATIVE_RING =
  sig
    include COMMUTATIVE_RING_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : ABELIAN_MONOID where type t = t
  end

signature INTEGRAL_DOMAIN =
  sig
    include INTEGRAL_DOMAIN_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : ABELIAN_MONOID where type t = t
  end

signature EUCLIDEAN_DOMAIN =
  sig
    include EUCLIDEAN_DOMAIN_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : ABELIAN_MONOID where type t = t
  end

signature FIELD =
  sig
    include FIELD_
    structure Addition : ABELIAN_GROUP where type t = t
    structure Multiplication : ABELIAN_GROUP where type t = t
  end
