local
  open Word5
in
  fun toBinary 0w0 = "0"
    | toBinary 0w1 = "1"
    | toBinary x = toBinary (x div 0w2) ^ toBinary (x mod 0w2)
  
  fun ofRevBinary nil = 0w0
    | ofRevBinary (#"0" :: l) = ofRevBinary l * 0w2
    | ofRevBinary (#"1" :: l) = ofRevBinary l * 0w2 + 0w1
    | ofRevBinary _ = raise Fail "bad binary number"
end

fun ps nil = print "\n"
  | ps (x :: y) = (print x; print " "; ps y)

val ofString = SOME o ofRevBinary o rev o String.explode
fun default y = fn SOME x => x | NONE => y

val (c, d) = case CommandLine.arguments () of
    a :: b :: _ => (ofString a, ofString b)
  | a :: _      => (ofString a, NONE)
  | _           => (NONE,       NONE)

structure B = FieldDollar(Galois5)
open B

val a = default 0wx2 c
val b = default 0wx4 d
val c = a *$ b
val d = !$ a

val _ = ps [ toBinary a, "** 19 =", toBinary (a **$ 19) ] 
val _ = ps [ toBinary a, "*", toBinary b, "=", toBinary c ]
val _ = ps [ toBinary a, "^-1 =", toBinary d ]
val _ = ps [ toBinary a, "*", toBinary d, "=", toBinary (a*$d) ]
