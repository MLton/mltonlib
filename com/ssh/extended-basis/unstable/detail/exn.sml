(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Exn : EXN = struct
   open Exn Ext.Exn
   val name = General.exnName
   val message = General.exnMessage
end
