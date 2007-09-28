(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the domain of the {MkUnitTest} functor.
 *)
signature MK_UNIT_TEST_DOM = sig
   include GENERIC
   include ARBITRARY sharing Open.Rep = ArbitraryRep
   include EQ        sharing Open.Rep = EqRep
   include PRETTY    sharing Open.Rep = PrettyRep
   include SHRINK    sharing Open.Rep = ShrinkRep
   include SIZE      sharing Open.Rep = SizeRep
end
