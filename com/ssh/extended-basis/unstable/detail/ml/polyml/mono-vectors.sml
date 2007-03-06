(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_VECTOR} modules for Poly/ML == *)

structure BoolVector : MONO_VECTOR = MkMonoVectorExt (BoolVector)
structure IntVector  : MONO_VECTOR = MkMonoVectorExt (IntVector)
structure RealVector : MONO_VECTOR = MkMonoVectorExt (RealVector)
