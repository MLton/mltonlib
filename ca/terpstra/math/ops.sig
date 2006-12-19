(****************************************************************** Groups *)

datatype order = FINITE of LargeInt.int | COUNTABLE | UNCOUNTABLE

signature SET =
  sig
    type t
    val order: order
    val EQ: (t * t) -> bool
  end

signature BINARY_OPERATION =
  sig
    include SET
    val MUL: (t * t) -> t
  end

signature ENDOFUNCTION =
  sig
    include BINARY_OPERATION
    type v
    val EVAL: t -> v -> v
  end
