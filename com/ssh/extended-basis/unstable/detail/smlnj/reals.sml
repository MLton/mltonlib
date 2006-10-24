(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {REAL} modules for SML/NJ == *)

structure Real = MkRealExt (Real)

structure LargeReal = MkRealExt (LargeReal)

structure Real64 = MkRealExt (Real64)
