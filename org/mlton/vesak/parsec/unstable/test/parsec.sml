(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Parsec =
   MkParsec (structure Sequence = StringSequence
             structure State = Unit)

val () = let
   open UnitTest Parsec

   infix |>>
   fun p |>> f = map f p

   fun parse p s =
       Parsec.parse p (StringSequence.full s, ())

   datatype 'a test =
      SUCCESS of String.t * 'a * String.t
    | FAILURE of String.t * Int.t

   fun remaining s =
       Substring.extract (StringSequence.vector s, StringSequence.pos s, NONE)
    >| Substring.string

   fun chk p t cs =
       test (fn () =>
       List.app
        (fn SUCCESS (s, v, r) =>
            (case parse p s
              of INL p => fails ["Parse failed at ", Int.toString p]
               | INR (v', (r', ())) =>
                 (thatEq t {actual = v', expect = v}
                ; thatEq String.t {actual = remaining r', expect = r}))
          | FAILURE (s, c) =>
            (case parse p s
              of INL p => thatEq Int.t {actual = p, expect = c}
               | INR (v, (r, ())) =>
                 fails ["Parse succeed with ", Generic.show t v,
                        " at pos ", Int.toString (StringSequence.pos r),
                        " and remaining input ",
                        Generic.show String.t (remaining r)]))
        cs)

   fun S s v r = SUCCESS (s, v, r)
   fun F s p = FAILURE (s, p)

   val d = sat Char.isDigit
   val l = sat Char.isLower
   val u = sat Char.isUpper
in
   unitTests
    (title "Parsec")

    (chk (l <|> u) Char.t [F "0" 0, S "ab" #"a" "b", S "Ba" #"B" "a"])

    (chk (l >>* u) (Sq.t Char.t) [F "Ul" 0, S "lU-" (#"l", #"U") "-"])

    (chk (between l u d) Char.t [S "b9X-" #"9" "-", F "bX" 1])

    (chk (count l 3 |>> implode) String.t [S "abcdE" "abc" "dE", F "abC" 2])

    (chk (endBy l u |>> implode) String.t
         [S "-" "" "-", S "aXbY-" "ab" "-", F "aXbYc-" 5])
    (chk (endBy1 l u |>> implode) String.t
         [F "-" 0, F "o-" 1, S "aXbY-" "ab" "-", F "aXbYc-" 5])

    (chk (many (l >>* u |>> op ^ o Sq.map str) |>> concat) String.t
         [S "-" "" "-", S "aBcD-" "aBcD" "-", F "abC" 1])
    (chk (manyRev (l >>* u |>> op ^ o Sq.map str) |>> concat) String.t
         [S "-" "" "-", S "aBcD-" "cDaB" "-", F "abC" 1])
    (chk (many1 (l >>* u |>> op ^ o Sq.map str) |>> concat) String.t
         [F "-" 0, S "aBcD-" "aBcD" "-", F "abC" 1])

    (chk (opt (count l 2 |>> implode)) (Option.t String.t)
         [S "xy-" (SOME "xy") "-", S "-" NONE "-", F "bA" 1])

    (chk (l >>* peek u) (Sq.t Char.t) [S "lU-" (#"l", #"U") "U-", F "ab" 1])

    (chk (sepBy l u |>> implode) String.t
         [S "-" "" "-", S "aXb-" "ab" "-", F "aXbY" 4])
    (chk (sepBy1 l u |>> implode) String.t
         [F "-" 0, S "aXb-" "ab" "-", F "aXbY" 4])

    $
end
