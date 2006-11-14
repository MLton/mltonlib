(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {MONO_ARRAY_SLICE} modules for SML/NJ == *)

structure RealArraySlice =
   MkMonoArraySliceExt (structure MonoArraySlice = RealArraySlice)
structure Real64ArraySlice =
   MkMonoArraySliceExt (structure MonoArraySlice = Real64ArraySlice)
