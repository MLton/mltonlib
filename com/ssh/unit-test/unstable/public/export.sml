(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature UNIT_TEST = UNIT_TEST

(** == Exported Functors == *)

functor MkUnitTest (Arg : MK_UNIT_TEST_DOM) :
   UNIT_TEST
      where type ('a,     'x) Rep.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Rep.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Rep.p = ('a, 'k, 'x) Arg.Open.Rep.p =
   MkUnitTest (Arg)
(**
 * Creates a unit test module.
 *)
