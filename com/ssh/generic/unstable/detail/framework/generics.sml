(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Generics :> GENERICS = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure W = Word32

   structure Label = struct
      type t = W.t * String.t
      val toString = Pair.snd
      val hash = Pair.fst
   end

   structure Con = Label

   structure Record = String
   structure Tuple = Int

   local
      (* The idea here is to compute the hash of at most some fixed number
       * of characters non-recursively.  This allows MLton to constant
       * fold the computation given a large enough inlining threshold.
       * -inline 275 with -loop-passes 2 has worked; default is -inline
       * 60 and -loop-passes 1, at the time of writing.
       *)
      fun hash s = let
         fun S (hi as (h, i)) =
             if i < 0
             then hi
             else (h * 0w33 + W.fromInt (ord (String.sub (s, i))), i-1)
      in
         case S(S(S(S(S(S(S(S(0w5381, size s-1))))))))
          of (h, n) => h + W.fromInt n
      end
   in
      fun L s = (hash s, s)
      val C = L
   end
end
