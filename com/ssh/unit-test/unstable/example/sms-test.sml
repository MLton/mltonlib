(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* This is a translation of the Haskell code from
 *
 *   [http://www.foomongers.org.uk/videos/spj-typedriventestinginhaskell.html]
 *)

val () = let
   open Generic UnitTest

   local
      open Word8
   in
      val op << = op <<
      val op >> = op >>
      val op orb = op orb
      val op andb = op andb
   end

   val rec pack =
    fn []                         => []
     | [a]                        => [a << 0w1]
     | [a, b]                     => [a << 0w1 orb b >> 0w6,
                                      b << 0w2]
     | [a, b, c]                  => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3]
     | [a, b, c, d]               => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3 orb d >> 0w4,
                                      d << 0w4]
     | [a, b, c, d, e]            => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3 orb d >> 0w4,
                                      d << 0w4 orb e >> 0w3,
                                      e << 0w5]
     | [a, b, c, d, e, f]         => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3 orb d >> 0w4,
                                      d << 0w4 orb e >> 0w3,
                                      e << 0w5 orb f >> 0w2,
                                      f << 0w6]
     | [a, b, c, d, e, f, g]      => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3 orb d >> 0w4,
                                      d << 0w4 orb e >> 0w3,
                                      e << 0w5 orb f >> 0w2,
                                      f << 0w6 orb g >> 0w1,
                                      g << 0w7]
     | a::b::c::d::e::f::g::h::rs => [a << 0w1 orb b >> 0w6,
                                      b << 0w2 orb c >> 0w5,
                                      c << 0w3 orb d >> 0w4,
                                      d << 0w4 orb e >> 0w3,
                                      e << 0w5 orb f >> 0w2,
                                      f << 0w6 orb g >> 0w1,
                                      g << 0w7 orb h]
                                     @ pack rs

   fun mask7 w = w andb 0w127

   val rec unpack =
    fn [] => []
     | [a]                     => [mask7 (a >> 0w1)]
     | [a, b]                  => [mask7 (a >> 0w1),
                                   mask7 (b >> 0w2 orb a << 0w6)]
     | [a, b, c]               => [mask7 (a >> 0w1),
                                   mask7 (b >> 0w2 orb a << 0w6),
                                   mask7 (c >> 0w3 orb b << 0w5)]
     | [a, b, c, d]            => [mask7 (a >> 0w1),
                                   mask7 (b >> 0w2 orb a << 0w6),
                                   mask7 (c >> 0w3 orb b << 0w5),
                                   mask7 (d >> 0w4 orb c << 0w4)]
     | [a, b, c, d, e]         => [mask7 (a >> 0w1),
                                   mask7 (b >> 0w2 orb a << 0w6),
                                   mask7 (c >> 0w3 orb b << 0w5),
                                   mask7 (d >> 0w4 orb c << 0w4),
                                   mask7 (e >> 0w5 orb d << 0w3)]
     | [a, b, c, d, e, f]      => [mask7 (a >> 0w1),
                                   mask7 (b >> 0w2 orb a << 0w6),
                                   mask7 (c >> 0w3 orb b << 0w5),
                                   mask7 (d >> 0w4 orb c << 0w4),
                                   mask7 (e >> 0w5 orb d << 0w3),
                                   mask7 (f >> 0w6 orb e << 0w2)]
     | a::b::c::d::e::f::g::rs => if mask7 g = 0w0
                                  then [mask7 (a >> 0w1),
                                        mask7 (b >> 0w2 orb a << 0w6),
                                        mask7 (c >> 0w3 orb b << 0w5),
                                        mask7 (d >> 0w4 orb c << 0w4),
                                        mask7 (e >> 0w5 orb d << 0w3),
                                        mask7 (f >> 0w6 orb e << 0w2),
                                        mask7 (g >> 0w7 orb f << 0w1)]
                                  else [mask7 (a >> 0w1),
                                        mask7 (b >> 0w2 orb a << 0w6),
                                        mask7 (c >> 0w3 orb b << 0w5),
                                        mask7 (d >> 0w4 orb c << 0w4),
                                        mask7 (e >> 0w5 orb d << 0w3),
                                        mask7 (f >> 0w6 orb e << 0w2),
                                        mask7 (g >> 0w7 orb f << 0w1),
                                        mask7 g]
                                       @ unpack rs

   fun thatId t f x = thatEq t {expect = x, actual = f x}
   val propPack = thatId (list word8) (unpack o pack) o map mask7
in
   unitTests
      (title "SMS.Manual")

      (test (fn () => let
          val isoBytes =
              Word8Vector.isoList <--> (Byte.stringToBytes, Byte.bytesToString)
          val testString = thatId string (Fn.map isoBytes (unpack o pack))
       in
          app testString
              ["",
               "1",
               "12",
               "123",
               "1234",
               "12345",
               "123456",
               "1234567",
               "12345678",
               "123456789",
               "1234567890"]
       end))

      (title "SMS.Generated")

      (testAll (list word8) propPack)
      (testAll (withGen let open RandomGen in list word8 8 end
                        (list word8))
               propPack)

      $
end
