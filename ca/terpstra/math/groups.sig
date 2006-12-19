signature SEMIGROUP =
  sig
    include BINARY_OPERATION
    (* a*(b*c) = (a*b)*c *)
    val associative : unit 
  end

signature MONOID =
  sig
    include SEMIGROUP
    
    (* one*x = x*one = x *)
    val one : t
  end

signature ABELIAN_MONOID =
  sig
    include MONOID
    
    (* a*b = b*a *)
    val commutative : unit
  end

signature GROUP =
  sig
    include MONOID
    (* y * (y^-1) = (y^-1) * y = one *)
    val INV : t -> t
  end

signature PERMUTATION =
  sig
    include GROUP
    (* include ENDOFUNCTION *)
    type v
    val EVAL: t -> v -> v
  end
signature AUTOFUNCTION = PERMUTATION

signature ABELIAN_GROUP =
  sig
    include GROUP
    
    (* a*b = b*a *)
    val commutative : unit
    
    (* x/y = x*(y^-1)) -- could be defined in GROUP, but is confusing *)
    val DIV : (t * t) -> t
  end

signature CYCLIC_GROUP =
  sig
    include ABELIAN_GROUP
    (* x = g^i *)
    val generator : t
  end

(****************************************************************** Unity *)

signature FINITE_SUBGROUPS =
  sig
    type t
    (* !!! Somehow figure out how to grab unity roots *)
  end

