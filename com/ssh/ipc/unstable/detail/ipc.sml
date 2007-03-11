(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure IPC : IPC = struct
   structure Type = RawMem.Type (* XXX hash type-indices for dynamic checking *)
end
