(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for Poly/ML == *)

structure IntInf : INT_INF = MkIntInfExt (IntInf)

structure Int32 : INTEGER = MkIntegerExt (Int32)
