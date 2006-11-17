(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Cmp :> CMP = struct
   type 'a t = 'a Sq.t -> Order.t

   fun mkRelOps cmp = let
      open Order
   in
      {<  = isLess    o cmp, <= = not o isGreater o cmp,
       == = isEqual   o cmp, != = not o isEqual   o cmp,
       >  = isGreater o cmp, >= = not o isLess    o cmp}
   end
end
