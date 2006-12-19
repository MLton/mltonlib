local
  open Word8
in
  fun toBinary 0w0 = "0"
    | toBinary 0w1 = "1"
    | toBinary x = toBinary (x div 0w2) ^ toBinary (x mod 0w2)
end

fun ps nil = print "\n"
  | ps (x :: y) = (print x; print " "; ps y)

fun default y = fn
   SOME x => x
 | NONE   => y

val toString = LargeInt.toString o Word32.toLargeIntX
val ofString = Option.compose (Word32.fromLargeInt, LargeInt.fromString)

val (c, d) = case CommandLine.arguments () of
    a :: b :: _ => (ofString a, ofString b)
  | a :: _      => (ofString a, NONE)
  | _           => (NONE,       NONE)

structure B = EuclideanDomainDollar(W32)
structure G = GCD(W32)
structure M = FieldPercent(Mersenne31)
open M
open B
open G

val a = default 0w4234235 c
val b = default 0w323432  d
val (g, i, j, _) = egcd (a, b)

val _ = ps [ 
  toString a, "*", toString i, "+", toString b, "*", toString j, 
  "=", toString g ]
val _ = ps [ toString a, "^-1 =", toString (!%a) ]

structure B = FieldDollar(Galois8)
open B

val x = 0wx02
val y = 0wx80
val z = x *$ y

val _ = ps [
  toBinary x, "*", toBinary y, "=", toBinary z ]
