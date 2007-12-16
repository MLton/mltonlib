(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure TextIO : TEXT_IO = struct
   open BasisTextIO

   local
      fun mk ln ss =
          (app (fn s => output (stdOut, s)) ss
         ; if ln then output1 (stdOut, #"\n") else ()
         ; flushOut stdOut)
   in
      val prints = mk false
      fun println s = mk true [s]
      val printlns = mk true
   end

   fun readFile file =
       case openIn file
        of s => Exn.after (fn () => inputAll s, fn () => closeIn s)

   fun writeFile {file, data} =
       case openOut file
        of s => Exn.after (fn () => output (s, data), fn () => closeOut s)
end
