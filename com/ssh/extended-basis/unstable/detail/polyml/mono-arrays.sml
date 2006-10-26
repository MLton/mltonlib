(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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

structure Word8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word8Array
                   structure MonoVector = Word8Vector)
