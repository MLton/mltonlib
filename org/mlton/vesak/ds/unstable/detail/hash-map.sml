(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure HashMap :> HASH_MAP = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   datatype ('a, 'b) t =
      IN of {table : {hash : Word.t,
                      key : 'a,
                      value : 'b Ref.t} Node.p Vector.t Ref.t,
             size : Int.t Ref.t,
             eq : 'a BinPr.t,
             hash : 'a -> Word.t}

   fun table (IN r) = !(#table r)
   fun size (IN r) = !(#size r)
   fun eq (IN r) = #eq r
   fun hash (IN r) = #hash r

   val caps = Vector.fromList
                 [3, 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191,
                  16381, 32749, 65521, 131071, 262139, 524287, 1048573,
                  2097143, 4194301, 8388593, 16777213, 33554393, 67108859,
                  134217689, 268435399, 536870909, 1073741789]
   val minCap = Vector.sub (caps, 0)
   val maxCap = Vector.sub (caps, Vector.length caps - 1)

   fun hashToIdx t hash =
       Word.toIntX (hash mod Word.fromInt (Vector.length (table t)))

   fun newTable cap = Vector.tabulate (cap, Node.ptr o ignore)

   fun locate t key' = let
      val hash' = hash t key'
      val idx = hashToIdx t hash'
   in
      (hash', Node.find (fn {hash, key, ...} =>
                            hash = hash' andalso eq t (key, key'))
                        (Vector.sub (table t, idx)))
   end

   fun maybeGrow (t as IN {size, table, ...}) = let
      val cap = Vector.length (!table)
   in
      if cap <= !size andalso cap < maxCap
      then let
            val newCap =
                recur 0 (fn lp =>
                         fn i => if Vector.sub (caps, i) = cap
                                 then Vector.sub (caps, i+1)
                                 else lp (i+1))
            val oldTable = !table
         in
            table := newTable newCap
          ; Vector.app (ignore o
                        Node.appClear
                           (fn c =>
                               Node.push
                                  (Vector.sub (!table, hashToIdx t (#hash c)))
                                  c))
                       oldTable
         end
      else ()
   end

   fun new {eq, hash} =
       IN {table = ref (newTable minCap),
           size  = ref 0,
           eq    = eq,
           hash  = hash}

   fun find t key' =
       case locate t key'
        of (_, INR p) => SOME (! (#value (Node.hd p)))
         | (_, INL _) => NONE

   fun insert (t as IN {size, ...}) (key, value) =
       case locate t key
        of (_,    INR p) => #value (Node.hd p) := value
         | (hash, INL p) =>
           (Node.push p {hash = hash, key = key, value = ref value}
          ; size := !size+1
          ; maybeGrow t)
end
