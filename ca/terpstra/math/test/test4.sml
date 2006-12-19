val toString = LargeInt.toString o Word32.toLargeIntX
val ofString = Option.compose (Word32.fromLargeInt, LargeInt.fromString)

val (c, d) = case CommandLine.arguments () of
    a :: b :: _ => (ofString a, ofString b)
  | a :: _      => (ofString a, NONE)
  | _           => (NONE,       NONE)

val a = getOpt (c, 0w4234235)
val b = getOpt (d, 0w323432)

fun ps nil = print "\n"
  | ps (x :: y) = (print x; print " "; ps y)

structure M = FieldPercent(Mersenne31)
structure L = DiscreteLogarithm(Mersenne31.Multiplication)
open M
open L

val ab = a *% b
val b' = ab /% a

val _ = ps [ toString a, "*", toString b, "=", toString ab ]
val _ = ps [ toString ab, "/", toString a, "=", toString b' ]

val g = 0w7 (* 3 is no generator -> multiple correct answers *)
val a = g **% 123
val x = log g a
val _ = ps [ "log", toString a, "=", LargeInt.toString x ]
val _ = ps [ toString g, "^", LargeInt.toString x, "=", toString (g **% x) ]
