(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TextIO = struct
   open BasisTextIO

   fun println s =
       (output (stdOut, s) ; output1 (stdOut, #"\n") ; flushOut stdOut)

   fun prints ss =
       (app (fn s => output (stdOut, s)) ss ; flushOut stdOut)

   fun readFile file =
       case openIn file
        of s => Exn.after (fn () => inputAll s, fn () => closeIn s)

   fun writeFile {file, data} =
       case openOut file
        of s => Exn.after (fn () => output (s, data), fn () => closeOut s)
end
