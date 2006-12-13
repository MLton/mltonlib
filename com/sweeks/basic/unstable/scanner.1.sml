(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Scanner = struct

   open Scanner

   fun map (s, f) =
      make (fn cs => Option.map (scan (s, cs), fn (x, cs) => (f x, cs)))

   fun scanString (s, str) =
      case scan (s, String.toSeq str) of
         None => None
       | Some (x, s) =>
            if Seq.isEmpty s then
               Some x
            else
               None

   fun ofBasis b =
      make (fn s => Option.ofBasis (b (Option.toBasis o Seq.get) s))

end
      
