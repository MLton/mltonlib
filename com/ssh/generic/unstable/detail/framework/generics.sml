(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generics :> GENERICS = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Label = struct
      open String
      val toString = id
   end

   structure Con = Label

   structure Record = Unit
   structure Tuple = Unit

   val L = id
   val C = id
end
