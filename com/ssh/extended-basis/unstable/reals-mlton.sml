(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(**
 * Extended {REAL} modules for MLton.
 *)

structure Real : REAL = MkRealExt (Real)

structure LargeReal : REAL = MkRealExt (LargeReal)

structure Real32 : REAL = MkRealExt (Real32)
structure Real64 : REAL = MkRealExt (Real64)
