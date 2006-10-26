(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for Poly/ML == *)

structure Int      : INTEGER = MkIntegerExt (Int)
structure LargeInt : INTEGER = MkIntegerExt (LargeInt)
structure Position : INTEGER = MkIntegerExt (Position)

structure IntInf : INT_INF = MkIntInfExt (IntInf)
