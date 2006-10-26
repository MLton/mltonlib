(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {REAL} modules for Poly/ML == *)

structure Real      : REAL = MkRealExt (Real)
structure LargeReal : REAL = MkRealExt (LargeReal)
