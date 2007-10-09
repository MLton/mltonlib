(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor ClosePrettyWithExtra (Arg : PRETTY_CASES) : GENERIC_EXTRA = struct
   structure Rep = CloseCases (Arg)
   structure Rep = WithExtra (open Arg Rep)
   open Arg Rep
   local
      (* <-- SML/NJ workaround *)
      open TopLevel
      (* SML/NJ workaround --> *)
      val et = C "&"
   in
      fun op &` ab =
          iso (data (Pretty.infixL 0 et ab
                     (C1 et (tuple2 ab))))
              (fn op & ? => ?, op &)
   end
end
