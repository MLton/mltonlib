(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnitTest :> UNIT_TEST = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix <$> <^> <\ >| &
   infixr @` |<
   (* SML/NJ workaround --> *)

   open Cvt Generic Prettier

   val format = let open Fmt in default & realFmt := StringCvt.GEN (SOME 16) end
   fun pretty t = fmt t format

   fun named t n v = group (nest 2 (str n <$> pretty t v))
   val strs = str o concat
   local
      open Maybe
      val I = Int.fromString
      val cols = Monad.sum [S"-w"@`I, L"--width"@`I, E"COLUMNS"@`I, `70]
      fun opt s l d = ref (valOf (get (Monad.sum [S s @` I, L l @`I, `d])))
   in
      val println = println (get cols)
      val defMaxPass = opt "-p" "--max-pass" 100
      val defMaxSkip = opt "-s" "--max-skip" 100
   end

   datatype t' =
      IN of {title : String.t Option.t,
             idx : Int.t}
   type t = (t', t', Unit.t) Fold.t
   type 'a s = (t, t, 'a) Fold.s

   exception Failure of Prettier.t
   fun failure d = raise Failure d

   val defaultCfg =
       IN {title = NONE,
           idx = 1}

   val succeeded = ref 0
   val failed = ref 0

   fun inc r = r := !r + 1

   val printlnStrs = println o group o strs

   (* We assume here that we're the first call to atExit so that it
    * is (relatively) safe to call terminate in our atExit effect.
    *)
   val () =
       OS.Process.atExit
          (fn () =>
              if 0 = !failed then
                 printlnStrs ["All ", D (!succeeded), " tests succeeded."]
              else
                 (printlnStrs [D (!succeeded + !failed), " tests of which\n",
                               D (!succeeded), " succeeded and\n",
                               D (!failed), " failed."]
                ; OS.Process.terminate OS.Process.failure))

   fun namedExn label e =
       named exn label e <^> dot <$>
       (case Exn.history e
         of [] => str "No exception history available"
          | hs => nest 2 (sep (str "Exception history:" ::
                               punctuate comma (map str hs))))

   fun unitTests ? = Fold.wrap (defaultCfg, ignore) ?
   fun title title = Fold.mapSt (const (IN {title = SOME title, idx = 1}))

   fun thatEq t {actual, expect} =
       if eq t (actual, expect) then ()
       else failure (nest 2 (sep [str "equality test failed:",
                                  named t "expected" expect <^> comma,
                                  named t "but got" actual]))

   fun that b = thatEq bool {expect = true, actual = b}
   fun thatNot b = thatEq bool {expect = false, actual = b}

   fun thatRaises exnPr th =
       try (th,
            fn _ => failure (str "didn't get an exception as expected"),
            fn e => if exnPr e then ()
                    else failure (namedExn "got an unexpected exception" e))
   fun thatRaises' exnEf =
       thatRaises (fn e => (exnEf e : Unit.t ; true) handle Match => false)
   fun thatFails ? = thatRaises (const true) ?

   (* TEST REGISTRATION INTERFACE *)

   fun test body =
       Fold.mapSt
          (fn IN {title, idx} =>
              (printlnStrs (case title
                             of NONE   => ["An untitled test"]
                              | SOME t => [D idx, ". ", t, " test"])
             ; try (body,
                    fn () =>
                       inc succeeded,
                    fn e =>
                       (inc failed
                      ; println
                        (indent 2
                         (txt "FAILED:" <$>
                          indent 2
                          (case e
                            of Failure d => d
                             | _ => namedExn "with exception" e) <^> dot))))
             ; IN {title = title, idx = idx + 1}))

   fun testEq t th = test (thatEq t o th)

   fun testRaises' exnEf th = test (fn () => thatRaises' exnEf th)
   fun testRaises exnPr th = test (fn () => thatRaises exnPr th)
   fun testFails th = test (fn () => thatFails th)

   datatype 'a result =
      BUG of 'a * Prettier.t
    | OK
    | SKIP

   local
      open RandomGen.RNG Maybe
      val W = Word.fromString
      val rng =
          ref o make o Seed.fromWord |<
          getOpt (get (Monad.sum [S"-s"@`W, L"--seed"@`W,
                                  mk RandomDev.seed ()]),
                  0w0)
   in
      fun nextRNG () = !rng before Ref.modify next rng
   end

   exception Skip

   fun allParam {size, maxPass, maxSkip} t ef = let
      fun test v =
          (ef v : Unit.t ; OK)
          handle Skip      => SKIP
               | Failure d => BUG (v, named t "with" v <$> d)
               | e         => BUG (v, named t "with" v <$> namedExn "raised" e)

      fun genTest passN =
          test (RandomGen.generate (size passN) (nextRNG ()) (arbitrary t))

      fun minimize (v, ms) = let
         fun lp []      = failure ms
           | lp (v::vs) =
             case test v
              of BUG (v, ms) => minimize (v, ms)
               | _           => lp vs
      in
         lp (shrink t v)
      end

      fun find (passN, skipN) =
          if maxPass <= passN then
             ()
          else if maxSkip <= skipN then
             println (indent 2 (strs ["Arguments exhausted after ", D passN,
                                      " tests."]))
          else
             case genTest (size passN)
              of SKIP =>
                 find (passN, skipN + 1)
               | OK =>
                 find (passN + 1, skipN)
               | BUG (v, ms) =>
                 minimize (v, ms)

      fun flet (r, v) th =
          case !r of v' => (r := v ; after (th, fn () => r := v'))
   in
      flet (defMaxPass, 1 + IntInf.log2 (IntInf.fromInt (!defMaxPass)))
           (fn () =>
               flet (defMaxSkip, 1 + !defMaxSkip div 2)
                    (fn () =>
                        find (0, 0)))
   end

   fun all t ef =
      allParam {size = fn n => n div 2 + 3,
                maxPass = !defMaxPass,
                maxSkip = !defMaxSkip}
               t ef

   fun testAll t ef = test (fn () => all t ef)

   fun skip () = raise Skip

   fun table t =
       case length t
        of n =>
           (punctuate comma o
            map (fn (n, m) => str (concat [D n, "% ", m])) o
            List.sort (Int.compare o Pair.swap o Pair.map (Sq.mk Pair.fst)) o
            map (Pair.map (fn l => Int.quot (100 * length l, n), hd) o Sq.mk) o
            List.divideByEq op = |< List.map (render NONE) t)

   type table = Prettier.t List.t Ref.t
   fun withFreq tblEf =
       case ref []
        of tbl => (tblEf tbl : Unit.t
                 ; println (indent 2 (nest 2 (sep (str "Statistics:" ::
                                                   table (!tbl)))) <^> dot))
   fun collect t tbl x =
       List.push tbl (pretty t x)
end
