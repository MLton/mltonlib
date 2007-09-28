(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Cmp :> CMP = struct
   open Cmp

   infix &

   fun map b2a = Fn.map (Sq.map b2a, Fn.id)

   fun op *` (aO, bO) (lA & lB, rA & rB) =
       case aO (lA, rA)
        of EQUAL => bO (lB, rB)
         | other => other

   local
      open Order
   in
      fun mkRelOps cmp =
          {<  = isLess    o cmp, <= = not o isGreater o cmp,
           == = isEqual   o cmp, != = not o isEqual   o cmp,
           >  = isGreater o cmp, >= = not o isLess    o cmp}

      local
         fun mk is cmp (x, y) = if is (cmp (x, y)) then y else x
      in
         fun max ? = mk isLess ?
         fun min ? = mk isGreater ?
      end
   end
end
