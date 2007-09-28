(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure UnitTest = MkUnitTest
  (open Generic
   structure ArbitraryRep = Open.Rep and EqRep = Open.Rep
         and PrettyRep = Open.Rep and ShrinkRep = Open.Rep
         and SizeRep = Open.Rep)
