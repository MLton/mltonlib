(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Cmp :> CMP = struct
   type 'a t = 'a Sq.t -> Order.t

   fun map b2a = Fn.map (Sq.map b2a, Fn.id)

   local
      open Order
   in
      fun mkRelOps cmp =
          {<  = isLess    o cmp, <= = not o isGreater o cmp,
           == = isEqual   o cmp, != = not o isEqual   o cmp,
           >  = isGreater o cmp, >= = not o isLess    o cmp}

      fun max cmp (x, y) = if isLess (cmp (x, y)) then y else x
      fun min cmp (x, y) = if isGreater (cmp (x, y)) then y else x
   end
end
