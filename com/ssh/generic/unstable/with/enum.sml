(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature Generic = sig
   include Generic ENUM
end

functor MkGeneric (Arg : Generic) = struct
   structure Open = MkGeneric (Arg)
   open Arg Open
   structure EnumRep = Open.Rep
end

structure Generic =
   MkGeneric (structure Open = WithEnum (Generic)
              open Generic Open)
