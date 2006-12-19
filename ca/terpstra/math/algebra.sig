signature SCALAR_MULTIPLY = 
  sig
    type e
    type t
    
    (* if one exists: one*v = v*one = v *)
    val MUL: e * t -> t
    
    (* a*(b*v) = (a*.b)*v *)
    val associative : unit
    
    (* c*(v+w) = c*v + v*w
     * (c+.d)*v = c*v + d*v
     *)
    val distributive : unit
  end

(* All MODULEs here are left modules *)
signature MODULE =
  sig
    type e 
    type t
    structure Base : RING where type t = e
    structure Addition : ABELIAN_GROUP where type t = t
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
  end

signature UNITARY_MODULE =
  sig
    type e 
    type t
    structure Base : UNITARY_RING where type t = e
    structure Addition : ABELIAN_GROUP where type t = t
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
  end

(* same as a UNITARY_MODULE, but over a field *)
signature VECTOR_SPACE =
  sig
    type e 
    type t
    structure Base : FIELD where type t = e
    structure Addition : ABELIAN_GROUP where type t = t
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
  end

signature ALGEBRA =
  sig
    include NON_ASSOCIATIVE_RING
    type e 
    structure Base : COMMUTATIVE_RING where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    
    (* (ax)y = a(xy)
     * a(xy) = x(ay)
     *)
    val bilinear : unit
  end

signature FIELD_ALGEBRA =
  sig
    include NON_ASSOCIATIVE_RING
    type e 
    structure Base : FIELD where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
  end

signature ASSOCIATIVE_ALGEBRA =
  sig
    include RING
    type e 
    structure Base : COMMUTATIVE_RING where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
  end

signature ASSOCIATIVE_FIELD_ALGEBRA =
  sig
    include RING
    type e 
    structure Base : FIELD where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
  end

signature UNITARY_ASSOCIATIVE_ALGEBRA =
  sig
    include UNITARY_RING
    type e 
    structure Base : COMMUTATIVE_RING where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
  end

signature UNITARY_ASSOCIATIVE_FIELD_ALGEBRA =
  sig
    include UNITARY_RING
    type e 
    structure Base : FIELD where type t = e
    structure ScalarMultiplication : SCALAR_MULTIPLY where type e = e and type t = t
    val bilinear : unit
  end
