(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_VECTOR} modules for Poly/ML == *)

structure BoolVector : MONO_VECTOR = MkMonoVectorExt (BoolVector)
structure IntVector  : MONO_VECTOR = MkMonoVectorExt (IntVector)
structure RealVector : MONO_VECTOR = MkMonoVectorExt (RealVector)
