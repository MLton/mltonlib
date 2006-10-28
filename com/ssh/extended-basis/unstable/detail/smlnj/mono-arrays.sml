(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY} modules for SML/NJ == *)

structure RealArray =
   MkMonoArrayExt (structure MonoArray = RealArray
                   structure MonoVector = RealVector)
structure Real64Array =
   MkMonoArrayExt (structure MonoArray = Real64Array
                   structure MonoVector = Real64Vector)
