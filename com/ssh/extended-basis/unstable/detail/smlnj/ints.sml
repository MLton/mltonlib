(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for SML/NJ == *)

structure FixedInt = MkIntegerExt (FixedInt)

structure Int31 = MkIntegerExt (Int31)
structure Int32 = MkIntegerExt (Int32)

structure Int64 = MkIntegerExt (Int64)

structure IntInf = MkIntInfExt (IntInf)
