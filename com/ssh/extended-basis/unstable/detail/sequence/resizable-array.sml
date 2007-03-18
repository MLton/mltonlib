(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure ResizableArray :> RESIZABLE_ARRAY = struct
   structure Buffer =
      MkBufferCommon (type 'a elem = 'a Option.t
                      val inj = SOME val prj = valOf fun any _ = NONE)
   open Buffer

   fun reserve b newCap =
       if newCap <= capacity b then () else realloc NONE b newCap

   fun pop t = let
      val n = length t - 1
   in
      if n < 0 then NONE else let
         val a = array t
         val result = A.sub (a, n)
      in
         A.update (a, n, NONE)
       ; set#length t n
       ; if n*3 < capacity t then realloc NONE t (capacity t div 2) else ()
       ; result
      end
   end
end
