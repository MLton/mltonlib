(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for MLKit == *)

structure Int31 : INTEGER = MkIntegerExt (Int31)
structure Int32 : INTEGER = MkIntegerExt (Int32)

structure IntInf : INT_INF = MkIntInfExt (IntInf)
