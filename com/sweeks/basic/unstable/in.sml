(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure In: IN = struct

   type t = TextIO.instream

   local
      open TextIO
   in
      val close = closeIn
      val get1 = Option.ofBasis o input1
      val getAll = inputAll
      val getLine = Option.ofBasis o inputLine
      val standard = stdIn
   end

   fun lines ins =
      Seq.unfold ((), fn () => Option.map (getLine ins, fn s => (s, ())))

end
