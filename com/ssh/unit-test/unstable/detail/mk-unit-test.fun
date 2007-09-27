(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkUnitTest (Arg : MK_UNIT_TEST_DOM) :>
   UNIT_TEST
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Open.Rep.p =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix <$> <^> <\ >| &
   infixr @` |<
   (* SML/NJ workaround --> *)

   open Arg Prettier

   structure Rep = Open.Rep

   fun sizeOf t v = Arg.sizeOf t v handle _ => 0
   fun named t n v = group (nest 2 (str n <$> pretty t v))
   val strs = str o concat
   local
      open Maybe
      val I = Int.fromString
      val cols = Monad.sum [S"-w"@`I, L"--width"@`I, E"COLUMNS"@`I, `70]
   in
      val println = println (get cols)
   end

   val i2s = Int.toString

   datatype t =
      IN of {title : String.t Option.t,
             idx : Int.t}
   type 'a s = (t, t, Unit.t, t, t, Unit.t, 'a) Fold.s

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
                 printlnStrs ["All ", i2s (!succeeded), " tests succeeded."]
              else
                 (printlnStrs [i2s (!succeeded + !failed), " tests of which\n",
                               i2s (!succeeded), " succeeded and\n",
                               i2s (!failed), " failed."]
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
                              | SOME t => [i2s idx, ". ", t, " test"])
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

   datatype result =
      BUG of Int.t * Prettier.t
    | OK
    | SKIP

   local
      open RandomGen.RNG
      val rng =
          ref (make (Seed.fromWord let
                        open Maybe
                        val W = Word.fromString
                     in
                        getOpt (get (Monad.sum [S"-s"@`W, L"--seed"@`W,
                                                mk RandomDev.seed ()]),
                                0w0)
                     end))
   in
      fun nextRNG () = !rng before Ref.modify next rng
   end

   exception Skip

   fun allParam {size, maxPass, maxSkip} t ef = let
      fun genTest passN = let
         val v = RandomGen.generate (size passN) (nextRNG ()) (arbitrary t)
      in
         (ef v : Unit.t ; OK)
         handle Skip      => SKIP
              | Failure d => BUG (sizeOf t v, named t "with" v <$> d)
              | e         => BUG (sizeOf t v,
                                  named t "with" v <$> namedExn "raised" e)
      end

      fun minimize (genSz, origSz, minSz, minMsg) =
          if genSz < 0
          then failure minMsg
          else case genTest genSz
                of BUG (sz, msg) =>
                   if sz < minSz
                   then minimize (genSz-1, origSz, sz, msg)
                   else minimize (genSz-1, origSz, minSz, minMsg)
                 | _ => minimize (genSz-1, origSz, minSz, minMsg)

      fun find (passN, skipN) =
          if maxPass <= passN then
             ()
          else if maxSkip <= skipN then
             println (indent 2 (strs ["Arguments exhausted after ", i2s passN,
                                      " tests."]))
          else
             case genTest (size passN)
              of SKIP =>
                 find (passN, skipN + 1)
               | OK =>
                 find (passN + 1, skipN)
               | BUG (sz, ms) =>
                 minimize (size passN, sz, sz, ms)
   in
      find (0, 0)
   end

   fun all t =
       allParam {size = fn n => n div 2 + 3,
                 maxPass = 100,
                 maxSkip = 100} t

   fun testAll t ef = test (fn () => all t ef)

   fun skip () = raise Skip

   fun table t = let
      val n = length t
   in
      punctuate comma o
      map (fn (n, m) => str (concat [i2s n, "% ", m])) o
      List.sort (Int.compare o Pair.swap o Pair.map (Sq.mk Pair.fst)) o
      map (Pair.map (fn l => Int.quot (100 * length l, n), hd) o Sq.mk) o
      List.divideByEq op = |< List.map (render NONE) t
   end

   type table = Prettier.t List.t Ref.t
   fun withFreq tblEf = let
      val tbl = ref []
   in
      tblEf tbl : Unit.t
    ; println (indent 2 (nest 2 (sep (str "Statistics:" :: table (!tbl)))) <^>
               dot)
   end
   fun collect t tbl x =
       List.push tbl (pretty t x)
end
