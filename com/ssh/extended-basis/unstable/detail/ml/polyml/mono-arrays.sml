(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {MONO_ARRAY} modules for Poly/ML == *)

structure BoolArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = BoolArray
                   structure MonoVector = BoolVector)

structure IntArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = IntArray
                   structure MonoVector = IntVector)

structure RealArray : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = RealArray
                   structure MonoVector = RealVector)
