(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Makes a cache module whose keys are int objects. *)
functor MkIntObjCache (Key : INT_OBJ) :> CACHE
      where type Key.t = Key.t = struct
   structure Key = Key

   datatype 'a t =
      IN of {size : Int.t Ref.t,
             table : {key : Key.t, value : 'a} Option.t Array.t Ref.t}
   fun gt s (IN r) = ! (s r)
   fun st s (IN r) v = s r := v

   exception NotFound

   fun size c = gt#size c
   fun cap c = Array.length (gt#table c)

   fun sub c i = valOf (Array.sub (gt#table c, i))
   fun update c (i, v) = Array.update (gt#table c, i, v)

   fun new () = IN {size = ref 0, table = ref (Array.tabulate (0, undefined))}

   fun realloc (IN {size, table}) newCap =
       table := Array.tabulate
                   (newCap,
                    fn i => if i < !size then Array.sub (!table, i) else NONE)

   fun putWith c k2v = let
      val n = size c
      val k = Key.new n
   in
      (if cap c = n then realloc c (n*2+1) else ()
     ; let val v = k2v k
       in st#size c (n+1)
        ; update c (n, SOME {key = k, value = v})
        ; (k, v)
       end)
      handle e => (Key.set k ~1 ; Key.discard k ; raise e)
   end

   fun put c = #1 o putWith c o const
   fun get c k = let
      val i = Key.get k
   in
      if i<0 orelse size c <= i then raise NotFound else let
         val {value, key} = sub c i
      in
         if k <> key then raise NotFound else value
      end
   end
   fun rem c k = let
      val n = size c - 1
      val i = Key.get k
      val r = sub c n
   in
      if i<0 orelse n<i orelse #key (sub c i) <> k then raise NotFound else ()
    ; Key.set k ~1 ; Key.discard k
    ; update c (i, SOME r)
    ; Key.set (#key r) i
    ; update c (n, NONE)
    ; st#size c n
    ; if n*4 < cap c then realloc c (cap c div 2) else ()
   end
   fun use c k = get c k before rem c k
end
