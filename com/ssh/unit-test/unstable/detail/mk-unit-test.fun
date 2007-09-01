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
   infix <^> <\ >| &
   infixr @` |<
   (* SML/NJ workaround --> *)

   structure G=Arg.RandomGen and I=Int

   structure Rep = Arg.Open.Rep

   local
      open Arg
   in
      val arbitrary = arbitrary
      val bool = bool
      val eq = eq
      val exn = exn
      val layout = layout
   end

   local
      open Prettier
   in
      val indent = nest 2 o sep
      fun named t n v = str n <^> nest 2 (line <^> layout t v)
      val comma = comma
      val dot = dot
      val group = group
      val op <^> = op <^>
      val pretty = pretty

      local
         open Maybe
         val I = I.fromString
         val cols = Monad.sum [S"-w"@`I, L"--width"@`I, E"COLUMNS"@`I, `70]
      in
         val println = println TextIO.stdOut (get cols)
      end

      val punctuate = punctuate
      val str = str
   end

   datatype t =
      IN of {title : String.t Option.t,
             idx : Int.t,
             size : Int.t UnOp.t,
             passM : Int.t,
             skipM : Int.t}
   type 'a s = (t, t, Unit.t, t, t, Unit.t, 'a) Fold.s

   exception Failure of Prettier.t
   fun failure ? = Exn.throw (Failure ?)

   val defaultCfg =
       IN {title = NONE,
           idx   = 1,
           size  = fn n => n div 2 + 3,
           passM = 100,
           skipM = 200}

   local
      val ~ = (fn {title=a, idx=b, size=c, passM=d, skipM=e} => a&b&c&d&e,
               fn a&b&c&d&e => {title=a, idx=b, size=c, passM=d, skipM=e})
      open FRU
   in
      val U = U
      fun updCfg ? = fruData (fn IN ? => ?, IN) A5 $ ~ ~ ?
   end

   val succeeded = ref 0
   val failed = ref 0

   val i2s = I.toString

   fun inc r = r := !r + 1

   fun runTest safeTest =
       Fold.mapSt (fn cfg as IN {idx, ...} =>
                      (inc (if safeTest cfg then succeeded else failed)
                     ; updCfg (U#idx (idx + 1)) $ cfg))

   fun header (IN {title, idx, ...}) =
       case title of NONE   => "An untitled test"
                   | SOME t => concat [i2s idx, ". ", t, " test"]

   (* We assume here that we're the first call to atExit so that it
    * is (relatively) safe to call terminate in our atExit effect.
    *)

   val printlnStrs = println o group o str o concat
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

   (* TEST SPECIFICATION INTERFACE *)

   fun unitTests ? = Fold.wrap (defaultCfg, ignore) ?
   fun title title = Fold.mapSt (updCfg (U #idx 1) (U #title (SOME title)) $)

   (* AD HOC TESTING HELPERS *)

   fun verifyEq t {actual, expect} =
       if eq t (actual, expect) then ()
       else failure (indent [str "Equality test failed:",
                             named t "expected" expect <^> comma,
                             named t "but got" actual])

   fun verifyTrue  b = verifyEq bool {expect = true,  actual = b}
   fun verifyFalse b = verifyEq bool {expect = false, actual = b}

   fun verifyFailsWith ePr th =
       try (th,
            fn _ => failure (str "Test didn't raise an exception as expected"),
            fn e => if ePr e then ()
                    else failure o group |<
                            named exn "Test raised an unexpected exception" e)

   fun verifyFails ? = verifyFailsWith (const true) ?
   fun verifyRaises e = verifyFailsWith (e <\ eq exn)

   (* TEST REGISTRATION INTERFACE *)

   fun history e =
       case Exn.history e
        of [] => str "No exception history available"
         | hs => indent (map str ("Exception history:"::hs))

   fun test body =
       runTest
          (fn cfg =>
              try (body,
                   fn _ =>
                      (printlnStrs [header cfg, " succeeded."]
                     ; true),
                   fn e =>
                      ((println o indent)
                          [str (header cfg ^ " failed."),
                           case e
                            of Failure doc => doc <^> dot
                             | _ => indent [str "Unhandled exception",
                                            str (Exn.message e) <^> dot],
                           history e <^> dot]
                     ; false)))

   fun testEq t th = test (verifyEq t o th)

   fun testTrue  th = test (verifyTrue  o th)
   fun testFalse th = test (verifyFalse o th)

   fun testFailsWith ep th = test (fn () => verifyFailsWith ep th)
   fun testFails th = test (fn () => verifyFails th)
   fun testRaises e th = test (fn () => verifyRaises e th)

   (* RANDOM TESTING INTERFACE *)

   type law = (Bool.t Option.t * String.t List.t * Prettier.t List.t) G.t

   local
      fun mk field value = Fold.mapSt (updCfg (U field value) $)
   in
      fun sizeFn  ? = mk #size  ?
      fun maxPass ? = mk #passM ?
      fun maxSkip ? = mk #skipM ?
   end

   val rng = ref (G.RNG.make (G.RNG.Seed.fromWord let
                                 open Maybe
                                 val W = Word.fromString
                              in
                                 getOpt (get (Monad.sum [S"-s"@`W, L"--seed"@`W,
                                                         mk RandomDev.seed ()]),
                                         0w0)
                              end))

   fun sort ? = SortedList.stableSort #n ?

   fun table n =
       punctuate comma o
       map (fn (n, m) => str (concat [i2s n, "% ", m])) o
       sort (I.compare o Pair.swap o Pair.map (Sq.mk Pair.fst)) o
       map (Pair.map (fn l => Int.quot (100 * length l, n), hd) o Sq.mk) o
       List.divideByEq op =

   fun chk prop =
       runTest
          (fn cfg as IN {size, passM, skipM, ...} => let
              fun done msg passN tags =
                  ((println o indent)
                      ((str o concat)
                          [header cfg, ":\n", msg, " ", i2s passN,
                           " random cases passed."]::
                       (if null tags then
                           []
                        else
                           [indent (str "Statistics:" ::
                                    table passN tags) <^> dot]))
                 ; true)

              fun lp passN skipN allTags =
                  if passM <= passN then
                     done "OK," passN allTags
                  else if skipM <= skipN then
                     done "Arguments exhausted after" passN allTags
                  else
                     case G.generate (size passN)
                                     (!rng before Ref.modify G.RNG.next rng)
                                     prop of
                        (NONE, _, _) =>
                        lp passN (skipN + 1) allTags
                      | (SOME true, tags, _) =>
                        lp (passN + 1) skipN (List.revAppend (tags, allTags))
                      | (SOME false, _, msgs) =>
                        ((println o indent)
                            [str (header cfg ^ " failed."),
                             indent (str "Falsifiable:"::msgs) <^> dot]
                       ; false)
           in
              lp 0 0 []
           end)

   fun all t toProp =
       G.>>= (arbitrary t,
              fn v => fn ? =>
                 (G.>>= (toProp v,
                         fn (r as SOME false, ts, msgs) =>
                            G.return (r, ts, named t "with" v :: msgs)
                          | p =>
                            G.return p) ?
                  handle e =>
                         G.return (SOME false, [],
                                   [named t "with" v,
                                    named exn "raised" e <^> dot,
                                    history e]) ?))

   fun that b = G.return (SOME b, [], [])
   val skip : law = G.return (NONE, [], [])

   fun classify tOpt p =
       G.Monad.map (fn p as (r, ts, msg) =>
                       case tOpt & r of
                          NONE & _ => p
                        | _ & NONE => p
                        | SOME t & _ => (r, t::ts, msg)) p
   fun trivial b = classify (if b then SOME "trivial" else NONE)

   fun collect t v p =
       G.Monad.map (fn (r, ts, msg) => (r, pretty NONE (layout t v)::ts, msg)) p
end
