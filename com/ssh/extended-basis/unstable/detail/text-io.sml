(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TextIO = struct
   open BasisTextIO

   fun println s =
       (output (stdOut, s) ; output1 (stdOut, #"\n") ; flushOut stdOut)
end
