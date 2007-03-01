(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * A simple unit testing framework.
 *)

structure UnitTest :> sig
   type t
   (** Type of unit test fold state. *)

   type 'a s = (t, t, t, Unit.t, 'a) Fold.step0
   (** Type of a unit test fold step. *)

   (** == TEST SPECIFICATION INTERFACE == *)

   val unitTests : (t, t, Unit.t, 'a) Fold.t
   (** Begins test specification. *)

   val title : String.t -> 'a s
   (** {title string} specifies the title for subsequent tests. *)

   (** === TEST REGISTRATION INTERFACE === *)

   val test : Unit.t Effect.t -> 'a s
   (**
    * Registers an ad hoc test.  An ad hoc test should indicate failure
    * by raising an exception.
    *)

   val testEq : 'a Type.t -> {actual : 'a, expect : 'a} Thunk.t -> 'b s
   (** Tests that the expected and actual values are equal. *)

   val testTrue  : Bool.t Thunk.t -> 'a s
   (** Tests that the thunk evaluates to {true}. *)

   val testFalse : Bool.t Thunk.t -> 'a s
   (** Tests that the thunk evaluates to {false}. *)

   val testFailsWith : Exn.t UnPr.t -> 'a Thunk.t -> 'b s
   (**
    * Tests that the thunk raises an exception satisfying the
    * predicate.
    *)

   val testFails : 'a Thunk.t -> 'b s
   (** Tests that the thunk raises an exception. *)

   val testRaises : Exn.t -> 'a Thunk.t -> 'b s
   (**
    * Tests that the thunk raises an exception equal to the given one.
    * The exception constructor must be registered with {Type.regExn}.
    *)

   (** == RANDOM TESTING INTERFACE == *)

   val sizeFn : Int.t UnOp.t -> 'a s
   (**
    * Sets the function to determine the "size" of generated random
    * test data.  The argument to the function is the number of tests
    * passed.  The default function is {fn n => n div 2 + 3}.
    *)

   val maxPass : Int.t -> 'a s
   (**
    * Sets the max number of passed random test cases to try per test.
    * The default is 100.
    *)

   val maxSkip : Int.t -> 'a s
   (**
    * Sets the max number of skipped random test cases to accept per
    * test.  The default is 200.  If a lot of tests are being skipped,
    * you should implement a better test data generator or a more
    * comprehensive law.
    *)

   type law
   (** The type of testable laws or properties. *)

   val chk : law -> 'b s
   (**
    * Tries to find counter examples to a given law by testing the law
    * with randomly generated cases.
    *)

   val all : 'a Type.t -> ('a -> law) -> law
   (**
    * Specifies that a law must hold for all values of type {'a}.  For
    * example,
    *
    *> all int (fn x => that (x = x))
    *
    * specifies that all integers must be equal to themselves.
    *)

   val that : Bool.t -> law
   (**
    * Specifies a primitive boolean law.  For example,
    *
    *> that (1 <= 2)
    *
    * specifies that {1} is less than or equal to {2}.
    *)

   val skip : law
   (**
    * Specifies that the premises of a conditional law aren't satisfied
    * so the specific test case of the law should be ignored.  For
    * example,
    *
    *> all (sq int)
    *>     (fn (x, y) =>
    *>         if x <= y then
    *>            that (Int.max (x, y) = y)
    *>         else
    *>            skip)
    *
    * specifies that if {x <= y} then {Int.max (x, y) = y}.
    *)

   val classify : String.t Option.t -> law UnOp.t
   (**
    * Classifies cases of a law.  The distribution of classified cases
    * will be logged.
    *)

   val trivial : Bool.t -> law UnOp.t
   (** Convenience function to classify cases of a law as "trivial". *)

   val collect : 'a Type.t -> 'a -> law UnOp.t
   (**
    * Classifies test cases by value of type {'a}.  The distribution as
    * well as the (pretty printed) values will be logged.
    *)

   (** == AD HOC TESTING HELPERS == *)

   exception Failure of Prettier.t
   (** Exception for reporting prettier errors. *)

   val verifyEq : 'a Type.t -> {actual : 'a, expect : 'a} Effect.t
   (** Verifies that the expected and actual values are equal. *)

   val verifyTrue : Bool.t Effect.t
   (** Verifies that the given value is {true}. *)

   val verifyFalse : Bool.t Effect.t
   (** Verifies that the given value is {false}. *)

   val verifyFailsWith : Exn.t UnPr.t -> 'a Thunk.t Effect.t
   (**
    * Verifies that the thunk fails with an exception satisfying the
    * predicate.
    *)

   val verifyFails : 'a Thunk.t Effect.t
   (** Verifies that the given thunk fails with an exception. *)

   val verifyRaises : Exn.t -> 'a Thunk.t Effect.t
   (**
    * Verifies that the thunk raises an exception equal to the given
    * one.  The exception constructor must be registered with
    * {Type.regExn}.
    *)
end = struct
   structure G = RanQD1Gen and I = Int and S = String

   local
      open Type
   in
      val arbitrary = arbitrary
      val bool = bool
      val eq = eq
      val exn = exn
      val layout = layout
      val notEq = notEq
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
   type 'a s = (t, t, t, Unit.t, 'a) Fold.step0

   exception Failure of Prettier.t

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
      fun updCfg ? = fruData (fn IN ? => ?, IN) A5 $ ~ ~ ?
   end

   val succeeded = ref 0
   val failed = ref 0

   val i2s = I.toString

   fun runTest safeTest =
       Fold.step0
          (fn cfg as IN {idx, ...} =>
              (if safeTest cfg then
                  succeeded += 1
               else
                  failed += 1
             ; updCfg (U#idx (idx + 1)) $ cfg))

   fun header (IN {title, idx, ...}) =
       if isSome title then
          concat [i2s idx, ". ", valOf title, " test"]
       else
          "An untitled test"

   (* We assume here that we're the first call to atExit so that it
    * is (relatively) safe to call terminate in our atExit effect.
    *)

   val printlnStrs = println o group o str o concat
   val () =
       OS.Process.atExit
          (fn () =>
              if 0 = !failed then
                 printlnStrs
                    ["All ", i2s (!succeeded), " tests succeeded."]
              else
                 (printlnStrs
                     [i2s (!succeeded + !failed), " tests of which\n",
                      i2s (!succeeded), " succeeded and\n",
                      i2s (!failed), " failed."]
                ; OS.Process.terminate OS.Process.failure))

   (* TEST SPECIFICATION INTERFACE *)

   fun unitTests ? =
       Fold.fold (defaultCfg, ignore) ?

   fun title title =
       Fold.step0 (updCfg (U #idx 1) (U #title (SOME title)) $)

   (* AD HOC TESTING HELPERS *)

   fun verifyEq t {actual, expect} =
       if notEq t (actual, expect) then
          raise Failure (indent [str "Equality test failed:",
                                 named t "expected" expect <^> comma,
                                 named t "but got" actual])
       else
          ()

   fun verifyTrue  b = verifyEq bool {expect = true,  actual = b}
   fun verifyFalse b = verifyEq bool {expect = false, actual = b}

   fun verifyFailsWith ePr th =
       try (th,
            fn _ =>
               raise Failure (str "Test didn't raise an\
                                  \ exception as expected"),
            fn e =>
               if ePr e then
                  ()
               else
                  raise Failure (group (named exn
                                              "Test raised an\
                                              \ unexpected exception"
                                              e)))

   fun verifyFails ? = verifyFailsWith (const true) ?
   fun verifyRaises e = verifyFailsWith (e <\ eq exn)

   (* TEST REGISTRATION INTERFACE *)

   fun test body =
       runTest
          (fn cfg =>
              try (body,
                   fn _ =>
                      (printlnStrs [header cfg, " succeeded."]
                     ; true),
                   fn e =>
                      (println
                          (indent
                              [str (header cfg ^ " failed."),
                               case e of
                                  Failure doc => doc <^> dot
                                | _ =>
                                  indent [str "Unhandled exception",
                                          str (Exn.message e) <^> dot],
                               case Exn.history e of
                                  [] =>
                                  str "No exception history available."
                                | hs => (indent o map str)
                                           ("Exception history:"::hs)])
                     ; false)))

   fun testEq t th = test (verifyEq t o th)

   fun testTrue  th = test (verifyTrue  o th)
   fun testFalse th = test (verifyFalse o th)

   fun testFailsWith ep th = test (fn () => verifyFailsWith ep th)
   fun testFails th = test (fn () => verifyFails th)
   fun testRaises e th = test (fn () => verifyRaises e th)

   (* RANDOM TESTING INTERFACE *)

   type law = (Bool.t Option.t * String.t List.t * Prettier.t List.t) G.gen

   local
      fun mk field value = Fold.step0 (updCfg (U field value) $)
   in
      fun sizeFn  ? = mk #size  ?
      fun maxPass ? = mk #passM ?
      fun maxSkip ? = mk #skipM ?
   end

   val rng = ref (G.make (Word32.fromWord (getOpt (RandomDev.seed (), 0w0))))

   fun chk prop =
       runTest
          (fn cfg as IN {size, passM, skipM, ...} => let
              fun sort ? = SortedList.stableSort #n ?

              fun group xs = let
                 fun lp (gs, xs) x =
                     fn y::ys =>
                        lp (if x = y then
                               (gs, x::xs)
                            else
                               ((x::xs)::gs, []))
                           y ys
                      | [] => (x::xs)::gs
              in
                 case sort S.compare xs of
                    [] => []
                  | x::xs => lp ([], []) x xs
              end

              fun table n allTags =
                  punctuate comma o
                  map (fn (n, m) => str (concat [i2s n, "% ", m])) o
                  sort (I.compare o Pair.swap o Pair.map (Sq.mk Pair.fst)) o
                  map (Pair.map (fn l => 100 * length l div n, hd) o Sq.mk) o
                  group |< sort S.compare allTags

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
                     case prop (size passN)
                               (!rng before Ref.modify G.next rng) of
                        (NONE, _, _) =>
                        lp passN (skipN + 1) allTags
                      | (SOME true, tags, _) =>
                        lp (passN + 1) skipN (List.revAppend (tags, allTags))
                      | (SOME false, _, msgs) =>
                        (println
                            (indent
                                [str (header cfg ^ " failed."),
                                 indent (str "Falsifiable:"::msgs)] <^>
                             dot)
                       ; false)
           in
              lp 0 0 []
           end)

   fun all t toProp =
       G.>>= (arbitrary t,
              fn v => fn n => fn g =>
                 try (fn () => toProp v n g,
                      fn (r as SOME false, ts, msgs) =>
                         (r, ts, named t "with" v :: msgs)
                       | p => p,
                      fn e => (SOME false, [],
                               [named t "with" v,
                                named exn "raised" e])))
   fun that b = G.return (SOME b, [], [])
   fun skip _ _ = (NONE, [], [])

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
