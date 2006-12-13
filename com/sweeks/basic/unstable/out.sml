(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Out: OUT = struct

   type t = TextIO.outstream

   local
      open TextIO
   in
      val close = closeOut
      val error = stdErr
      val flush = flushOut
      val put = output
      val put1 = output1
      val standard = stdOut
   end

   fun puts (out, ss) = Seq.for (ss, fn s => put (out, s))

   fun newline out = put1 (out, #"\n")

end

fun print s = Out.put (Out.standard, s)
