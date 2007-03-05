(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {REAL} modules for MLton == *)

structure Real32 : REAL = MkRealExt (BasisReal32)
structure Real64 : REAL = MkRealExt (BasisReal64)
