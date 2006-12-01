(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Extended {WORD} modules for MLKit == *)

structure SysWord : WORD = MkWordExt (SysWord)

structure Word31 : WORD = MkWordExt (Word31)
structure Word32 : WORD = MkWordExt (Word32)
