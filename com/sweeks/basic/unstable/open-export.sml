open Export

datatype z = datatype Bool.t
val _: z = false (* quell unused warning *)
datatype z = datatype List.t
val _: Unit.t z = [] (* quell unused warning *)
datatype z = datatype Order.t
val _: z = Less (* quell unused warning *)
datatype z = datatype Option.t
val _: Unit.t z = None (* quell unused warning *)
type z = Unit.t

(* The following are so that nice type names are used by -show-basis. *)
structure Int = Int
structure IntInf = IntInf
structure Real = Real
structure Word = Word
