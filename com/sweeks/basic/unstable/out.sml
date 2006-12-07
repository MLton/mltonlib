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
