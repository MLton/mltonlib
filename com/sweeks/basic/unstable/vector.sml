(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Vector: VECTOR = struct

   type 'a t = 'a Vector.t

   local
      open Vector
   in
      val sub = sub
      val size = length
   end

   structure Unsafe = struct
      val sub = Unsafe.Vector.sub
   end

   fun make n = let
      val a = Array.Unsafe.make n
      val subLim = ref 0
      fun sub i =
         if MLton.safe andalso Int.geu (i, !subLim) then
            raise Subscript
         else
            Array.Unsafe.sub (a, i)
      val updateLim = ref 0
      fun update (i, x) =
         if MLton.safe andalso Int.geu (i, !updateLim) then
            if i = !updateLim andalso i < n then
               (Array.Unsafe.update (a, i, x);
                subLim := i + 1;
                updateLim := i + 1)
            else
               raise Subscript
         else
            Array.Unsafe.update (a, i, x)
      val gotIt = ref false
      fun done () =
         if !gotIt then
            die "already got vector"
         else if n = !updateLim then
            (gotIt := true;
             updateLim := 0;
             Primitive.Array.toVector a)
         else
            die "vector not full"
   in
      {done = done,
       sub = sub,
       update = update}
   end

   local
      structure S = RamSequence (type 'a t = 'a t
                                 val fromArray = Primitive.Array.toVector
                                 val size = size
                                 val sub = sub)
   in
      open S
   end

end

type 'a vector = 'a Vector.t
