(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR} modules for SML/NJ == *)

structure RealVector = MkMonoVectorExt (BasisRealVector)
structure Real64Vector = MkMonoVectorExt (BasisReal64Vector)
