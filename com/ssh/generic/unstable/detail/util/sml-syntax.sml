(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Utilities for dealing with the syntax of Standard ML.
 *)
structure SmlSyntax :> sig
   (** == PREDICATES FOR IDENTIFIERS == *)

   val isAlphaNumId : String.t UnPr.t
   val isId         : String.t UnPr.t
   val isLabel      : String.t UnPr.t
   val isLongId     : String.t UnPr.t
   val isNumLabel   : String.t UnPr.t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   infix  2 andAlso
   infix  1 orElse
   (* SML/NJ workaround --> *)

   structure C = Char and L = List and S = String
   val isSym = C.contains "!%&$#+-/:<=>?@\\~`^|*"
   val isntEmpty = 0 <\ op < o size
   val isSymId = isntEmpty andAlso S.all isSym
   val isAlphaNumId = isntEmpty
                      andAlso C.isAlpha o S.sub /> 0
                      andAlso S.all (C.isAlphaNum
                                     orElse #"'" <\ op =
                                     orElse #"_" <\ op =)
   val isNumLabel = isntEmpty
                    andAlso #"0" <\ op <> o S.sub /> 0
                    andAlso S.all C.isDigit
   val isId = isAlphaNumId orElse isSymId
   val isLongId = L.all isId o S.fields (#"." <\ op =)
   val isLabel = isId orElse isNumLabel
end
