(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {INTEGER} and {INT_INF} modules for SML/NJ == *)

structure Int      = MkIntegerExt (Int)
structure FixedInt = MkIntegerExt (FixedInt)
structure LargeInt = MkIntegerExt (LargeInt)
structure Position = MkIntegerExt (Position)

structure Int31 = MkIntegerExt (Int31)
structure Int32 = MkIntegerExt (Int32)

structure Int64 = MkIntegerExt (Int64)

structure IntInf = MkIntInfExt (IntInf)
