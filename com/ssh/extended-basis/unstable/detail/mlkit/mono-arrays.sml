(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY} modules for MLKit == *)

structure Word8Array : MONO_ARRAY =
   MkMonoArrayExt (structure MonoArray = Word8Array
                   structure MonoVector = Word8Vector)
