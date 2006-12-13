(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Option = struct

   open Option

   val toSeq =
      fn None => Seq.empty ()
       | Some x => Seq.single x

end

