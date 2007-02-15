(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CeeCache :> CACHE where type Key.t = C.voidptr = struct
   structure Key = struct
      type t = C.voidptr

      val unused = ref [] (* XXX free these at some point *)
      fun new () =
          case List.pop unused of
             SOME v => v
           | NONE => C.Ptr.inject' (C.Ptr.|&! (C.new' C.S.sint))
      val free = unused <\ List.push
      val get = C.Get.sint' o C.Ptr.|*! o C.Ptr.cast'
      fun set k = C.Ptr.|*! (C.Ptr.cast' k) <\ C.Set.sint'
   end

   datatype 'a t =
      IN of {size : Int.t Ref.t,
             table : {key : Key.t, value : 'a} Option.t Array.t Ref.t}
   fun g s (IN r) = ! (s r)
   fun set s (IN r) v = s r := v

   exception NotFound

   fun size c = g#size c

   fun sub c i = valOf (Array.sub (g#table c, i))
   fun update c (i, v) = Array.update (g#table c, i, v)

   fun new () = IN {size = ref 0, table = ref (Array.tabulate (0, undefined))}

   fun ensureCapacity (IN {size, table}) reqCap = let
      val curCap = Array.length (!table)
   in
      if reqCap <= curCap
      then ()
      else table := Array.tabulate
                       (Int.max (reqCap, curCap*2+1),
                        fn i => if i < !size
                                then Array.sub (!table, i)
                                else NONE)
   end

   fun putWith c k2v = let
      val n = size c
      val k = Key.new ()
   in
      (ensureCapacity c (n+1)
     ; let val v = k2v k
       in set#size c (n+1)
        ; update c (n, SOME {key = k, value = v})
        ; Key.set k n
        ; (k, v)
       end)
      handle e => (Key.free k ; raise e)
   end

   fun put c = #1 o putWith c o const
   fun get c k = let
      val i = Key.get k
   in
      if size c <= i then raise NotFound else let
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
      if n < i orelse #key (sub c i) <> k then raise NotFound else ()
    ; Key.free k
    ; update c (i, SOME r)
    ; Key.set (#key r) i
    ; update c (n, NONE)
    ; set#size c n
   end
   fun use c k = get c k before rem c k
end
