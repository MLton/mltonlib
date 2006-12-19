structure B = FieldDollar(Galois4)
open B

fun table 16 = print "\n"
  | table y = (
    print ("0x" ^ Word4.toString (!$ (Word4.fromInt y)) ^ ", "); 
    table (y+1))

val _ = table 0

(*fun ps nil = print "\n"
  | ps (x :: y) = (print x; print " "; ps y)

fun default y = fn
   SOME x => x
 | NONE   => y

(*
val toString = LargeInt.toString
val ofString = LargeInt.fromString

val (d, e, f) = case CommandLine.arguments () of
    a :: b :: c :: _ => (ofString a, ofString b, ofString c)
  | a :: b :: _      => (ofString a, ofString b, NONE)
  | a :: _           => (ofString a, NONE,       NONE)
  | _                => (NONE,       NONE,       NONE)

structure G = GCD(Z)
structure B = EuclideanDomainDollar(Z)
open B
open G

val a = default 4234235 d
val b = default 32 e
val c = default 3242 f
val (g, i, j, _) = egcd (a, b)

val _ = ps [ 
  toString a, "*", toString i, "+", toString b, "*", toString j, 
  "=", toString g ]
val _ = ps [
  toString a, "**", toString b, "=", toString (a **$ b), "=",
  toString (a **$ b %$ c), "mod", toString c ]
*)

structure P = PolynomialOverField(Galois8)
structure B = Polynomial(P)
structure G = GCD(P)
open B
open G

(* For Q
fun toString p = (
  VectorSlice.foldl
    (fn ((x, y), a) => (a ^ " (" ^ LargeInt.toString x ^ ", " ^ LargeInt.toString y ^ ")")) "[" p
  ^ " ]")
*)

fun toString p =
  VectorSlice.foldl (fn (x, a) => (a ^ " " ^ Word8.toString x)) "[" p ^ " ]"

(* for Q
val x = P.fromList [ (4, 3), (7, 3), (2, 1), (7, 8), (9, 2) ]
val y = P.fromList [ (2, 1), (5, 3), (7, 4) ]
*)

(* for Galois8 *)
val x = P.fromList [ 0wx7A, 0wx2F, 0wx10, 0wx3D, 0wxF2, 0wx02, 0wxDA, 0wxE0 ]
val y = P.fromList [ 0wx40, 0wx2F, 0wxA2, 0wx89, 0wx62 ]

(* for R
val x = P.fromList [ 6.0, 2.0, 1.0, 9.0, 2.0, 5.0, 7.0, 11.0 ]
val y = P.fromList [ 4.0, 3.0, 5.0, 8.0, 2.0 ]
*)

val z = x *$ y
val _ = ps [ toString x, "*", toString y, "=", toString z ]

val (q, r) = x //$ y
val _ = ps [ toString q, "*", toString y, "+", toString r, "=", toString (q*$y+$r) ]

val (g, i, j, _) = egcd (x, y)
val _ = ps [ 
  toString x, "*", toString i, "+", toString y, "*", toString j, 
  "=", toString (x*$i+$y*$j), "=", toString g ]
*)
