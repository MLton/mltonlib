(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for a simple unit testing framework.
 *)
signature UNIT_TEST = sig
   structure Rep : OPEN_GENERIC_REP
   (** Substructure specifying the representation of generics. *)

   type t
   (** Type of unit test fold state. *)

   type 'a s = (t, t, Unit.t, t, t, Unit.t, 'a) Fold.s
   (** Type of a unit test fold step. *)

   (** == TEST SPECIFICATION INTERFACE == *)

   val unitTests : (t, t, Unit.t, 'a) Fold.f
   (** Begins test specification. *)

   val title : String.t -> 'a s
   (** {title string} specifies the title for subsequent tests. *)

   (** === TEST REGISTRATION INTERFACE === *)

   val test : Unit.t Effect.t -> 'a s
   (**
    * Registers an ad hoc test.  An ad hoc test should indicate failure by
    * raising an exception.
    *)

   val testEq : ('a, 'x) Rep.t -> {actual : 'a, expect : 'a} Thunk.t -> 'b s
   (** Tests that the expected and actual values are equal. *)

   val testTrue  : Bool.t Thunk.t -> 'a s
   (** Tests that the thunk evaluates to {true}. *)

   val testFalse : Bool.t Thunk.t -> 'a s
   (** Tests that the thunk evaluates to {false}. *)

   val testFailsWith : Exn.t UnPr.t -> 'a Thunk.t -> 'b s
   (** Tests that the thunk raises an exception satisfying the predicate. *)

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
    * Sets the function to determine the "size" of generated random test
    * data.  The argument to the function is the number of tests passed.
    * The default function is {fn n => n div 2 + 3}.
    *)

   val maxPass : Int.t -> 'a s
   (**
    * Sets the maximum number of passed random test cases to try per test.
    * The default is 100.
    *)

   val maxSkip : Int.t -> 'a s
   (**
    * Sets the maximum number of skipped random test cases to accept per
    * test.  The default is 200.  If a lot of tests are being skipped, you
    * should implement a better test data generator or a more
    * comprehensive law.
    *)

   type law
   (** The type of testable laws or properties. *)

   val chk : law -> 'b s
   (**
    * Tries to find counter examples to a given law by testing the law
    * with randomly generated cases.
    *)

   val all : ('a, 'x) Rep.t -> ('a -> law) -> law
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
    * Specifies that the premises of a conditional law aren't satisfied so
    * the specific test case of the law should be ignored.  For example,
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

   val collect : ('a, 'x) Rep.t -> 'a -> law UnOp.t
   (**
    * Classifies test cases by value of type {'a}.  The distribution as
    * well as the (pretty printed) values will be logged.
    *)

   (** == AD HOC TESTING HELPERS == *)

   exception Failure of Prettier.t
   (** Exception for reporting prettier errors. *)

   val verifyEq : ('a, 'x) Rep.t -> {actual : 'a, expect : 'a} Effect.t
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
    * Verifies that the thunk raises an exception equal to the given one.
    *)
end
