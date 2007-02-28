(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of the {WORD_TABLE} signature.  The table capacities
 * are primes.  The primes used are the largest primes less than 2^i for i
 * in {4, ..., 30}.  The table capacity is roughly doubled when the size
 * of the table is the capacity and roughly halved when the size of the
 * table is one quarter of the capacity.  This ensures that any sequence
 * of insertions and deletions is linear modulo collisions.
 *)

structure WordTable :> WORD_TABLE where type Key.t = Word32.t = struct
   structure Key = Word32 and W = Word32 and N = Node and V = Vector

   datatype 'a t = IN of {table : (W.t * 'a) N.t Vector.t Ref.t,
                          size : Int.t Ref.t}

   val caps = V.fromList
                 [3, 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191,
                  16381, 32749, 65521, 131071, 262139, 524287, 1048573,
                  2097143, 4194301, 8388593, 16777213, 33554393, 67108859,
                  134217689, 268435399, 536870909, 1073741789]
   val minCap = V.sub (caps, 0)
   val maxCap = V.sub (caps, V.length caps - 1)

   fun table (IN {table, ...}) = !table
   fun size (IN {size, ...}) = !size

   fun keyToIdx t key = W.toIntX (key mod W.fromInt (V.length (table t)))
   fun putAt t idx entry = N.push (V.sub (table t, idx)) entry
   fun newTable cap = V.tabulate (cap, N.new o ignore)
   fun findKey t idx key = N.find (key <\ op = o #1) (V.sub (table t, idx))

   fun maybeRealloc (t as IN {table, ...}) = let
      val cap = V.length (!table)
      fun findIdx cap = #1 (valOf (V.findi (cap <\ op = o #2) caps))
      fun realloc offs = let
         val newCap = V.sub (caps, findIdx cap + offs)
         val oldTable = !table
      in
         table := newTable newCap
         (* Theoretically speaking, it should be possible to
          * execute the following code in constant space.
          *)
       ; V.app (ignore o
                N.appClear
                   (fn entry as (key, _) => putAt t (keyToIdx t key) entry))
               oldTable
      end
   in
      if size t <= cap div 4 andalso minCap < cap then
         realloc ~1
      else if cap <= size t andalso cap < maxCap then
         realloc 1
      else
         ()
   end

   fun new () = IN {table = ref (newTable minCap),
                    size = ref 0}

   fun == (IN {table = l, ...}, IN {table = r, ...}) = l = r

   structure Action = struct
      type ('v, 'r) t = ((W.t * 'v) N.t,
                         (W.t * 'v) N.t) Sum.t * W.t * 'v t -> 'r
      type ('v, 'r, 's) m = ('v, 'r) t
      type none = unit
      type some = unit

      fun get {some, none} =
          fn s as (INL _, _, _) => none () s
           | s as (INR n, _, _) => some (Pair.snd (N.hd n)) s

      fun peek {some, none} =
          fn s as (INL _, _, _) => none () s
           | s as (INR _, _, _) => some () s

      fun insert value result =
          fn (INL n, key, t as IN {size, ...}) =>
             (size := !size + 1
            ; N.push n (key, value)
            ; maybeRealloc t
            ; result)
           | (INR _, _, _) =>
             undefined ()

      fun update value result =
          fn (INL _, _, _) =>
             undefined ()
           | (INR n, key, _) =>
             (N.<- (n, SOME ((key, value), N.tl n))
            ; result)

      fun remove result =
          fn (INL _, _, _) =>
             undefined ()
           | (INR n, _, t as IN {size, ...}) =>
             (size := !size - 1
            ; N.drop n
            ; maybeRealloc t
            ; result)

      val return = const
   end

   fun access t key action =
       action (findKey t (keyToIdx t key) key, key, t)
end
