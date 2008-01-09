(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* WARNING: This file is generated! *)

signature Generic = sig
   include Generic READ
end

functor MkGeneric (Arg : Generic) = struct
   structure Open = MkGeneric (Arg)
   open Arg Open
   structure ReadRep = Open.Rep
end

structure Generic =
   MkGeneric (structure Open = WithRead (Generic)
              open Generic Open)
