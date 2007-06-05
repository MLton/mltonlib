(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generics :> GENERICS = struct
   structure Label = struct
      type t = String.t
      val toString = id
   end

   structure Con = Label

   structure Record = Unit
   structure Tuple = Unit

   local
      fun mk p v = if p v then v else fail "syntax error"
   in
      val L = mk SmlSyntax.isLabel
      val C = mk SmlSyntax.isLongId
   end
end
