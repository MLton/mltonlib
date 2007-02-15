(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Unit tests for the {Promise} module.
 *)

val () = let
   open Type UnitTest

   val fix = Tie.fix

   local
      open Promise
   in
      val D = delay
      val E = eager
      val F = force
      val L = lazy
      val Y = Y
   end

   (* lazy stream *)
   datatype 'a stream' = NIL | CONS of 'a * 'a stream
   withtype 'a stream = 'a stream' Promise.t

   local
      fun strip s = case F s of NIL => raise Empty | CONS x => x
   in
      fun hd s = #1 (strip s)
      fun tl s = #2 (strip s)
   end

   fun cons x = E (CONS x)

   fun streamDrop (s, i) =
       L (fn () =>
             if 0 = i then
                s
             else
                streamDrop (tl s, i - 1))

   fun streamSub (s, i) = hd (streamDrop (s, i))

   (* helpers *)
   fun inc x = (x += 1 ; !x)
in
   unitTests
      (title "Promise.fix")

      (testRaises
          Fix.Fix
          (fn () =>
              fix Y (fn invalid =>
                        (F invalid ; E ()))))

      (testEq
          int
          (fn () => let
                 fun streamZipWith fxy (xs, ys) =
                     D (fn () =>
                           CONS (fxy (hd xs, hd ys),
                                 streamZipWith fxy (tl xs, tl ys)))

                 val fibs =
                     fix Y (fn fibs =>
                               0 </cons/> 1 </cons/>
                                 (streamZipWith
                                     op +
                                     (L (fn () => tl fibs), fibs)))
              in
                 {expect = 8,
                  actual = streamSub (fibs, 6)}
              end))

      (title "Promise - memoization")

      (testEq
          (list int)
          (fn () => let
                 val count = ref 0
                 val s = D (fn () => inc count)
              in
                 {expect = [1, 1, 1],
                  actual = [F s, F s, !count]}
              end))

      (testEq
          (list int)
          (fn () => let
                 val count = ref 0
                 val s = D (fn () => inc count)
              in
                 {expect = [2, 1],
                  actual = [F s + F s, !count]}
              end))

      (testEq
          (list int)
          (fn () => let
                 val count = ref 0
                 val r = D (fn () => inc count)
                 val s = L (Thunk.mk r)
                 val t = L (Thunk.mk s)
              in
                 {expect = [1, 1, 1],
                  actual = [F t, F r, !count]}
              end))

      (testEq
          (list int)
          (fn () => let
                 val count = ref 0
                 fun ones () = D (fn () => CONS (inc count, ones ()))
                 val s = ones ()
              in
                 {expect = [5, 5, 5],
                  actual = [streamSub (s, 4), streamSub (s, 4), !count]}
              end))

      (title "Promise - reentrancy")

      (testEq
          (list int)
          (fn () => let
                 val count = ref 0
                 val x = ref 5
                 val p = fix Y (fn p =>
                                   D (fn () =>
                                         if inc count > !x then
                                            !count
                                         else
                                            F p))
              in
                 {expect = [6, 6],
                  actual = [F p, (x := 10 ; F p)]}
              end))

      (testEq
          int
          (fn () => let
                 val first = ref true
                 val f = fix Y (fn f =>
                                   D (fn () =>
                                         if !first then
                                            (first := false ; F f)
                                         else
                                            2))
              in
                 {expect = 2,
                  actual = F f}
              end))

      (testEq
          (list int)
          (fn () => let
                 val count = ref 5
                 val p = fix Y (fn p =>
                                   D (fn () =>
                                         if !count <= 0 then
                                            !count
                                         else
                                            (count -= 1
                                           ; ignore (F p)
                                           ; count += 2
                                           ; !count)))
              in
                 {expect = [5, 0, 10],
                  actual = [!count, F p, !count]}
              end))

      $
end
