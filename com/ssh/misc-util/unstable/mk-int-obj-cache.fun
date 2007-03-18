(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Makes a cache module whose keys are int objects. *)
functor MkIntObjCache (Key : INT_OBJ) :> CACHE
      where type Key.t = Key.t = struct
   structure Key = Key and R = ResizableArray

   type 'a t = {key : Key.t, value : 'a} R.t

   exception NotFound

   val size = R.length
   val isEmpty = R.isEmpty
   val new = R.new

   fun putWith c k2v = let
      val n = size c
      val k = Key.new n
   in
      let
         val v = k2v k
      in
         R.push c {key = k, value = v}
       ; (k, v)
      end
      handle e => (Key.discard k ; raise e)
   end

   fun put c = #1 o putWith c o const
   fun get c k = let
      val i = Key.get k
   in
      if i < 0 orelse size c <= i then raise NotFound else let
         val {value, key} = R.sub (c, i)
      in
         if k <> key then raise NotFound else value
      end
   end
   fun rem c k = let
      val n = size c - 1
      val i = Key.get k
      val r = R.sub (c, n)
   in
      if i<0 orelse n<i orelse #key (R.sub (c, i)) <> k
      then raise NotFound else ()
    ; Key.discard k ; R.update (c, i, r) ; Key.set (#key r) i
    ; ignore (R.pop c)
   end
   fun use c k = get c k before rem c k
   fun values (c : 'a t) = List.tabulate (size c, #value o c <\ R.sub)
end
