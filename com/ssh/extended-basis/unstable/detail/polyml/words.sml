(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

(** == Extended {WORD} modules for Poly/ML == *)

structure Word      : WORD = MkWordExt (Word)
structure LargeWord : WORD = MkWordExt (LargeWord)
structure SysWord   : WORD = MkWordExt (SysWord)
structure Word8     : WORD = MkWordExt (Word8)
