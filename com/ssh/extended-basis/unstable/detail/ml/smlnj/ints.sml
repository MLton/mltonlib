(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for SML/NJ == *)

structure Int31 = MkIntegerExt (BasisInt31)
structure Int32 = MkIntegerExt (BasisInt32)

structure Int64 = MkIntegerExt (BasisInt64)

structure IntInf = MkIntInfExt (BasisIntInf)
