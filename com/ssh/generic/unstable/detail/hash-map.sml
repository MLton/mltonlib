(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Node :> sig
   type 'a t
   type 'a p = 'a t Option.t Ref.t

   val new : 'a -> 'a t
   val ptr : 'a p Thunk.t

   val next : 'a t -> 'a p
   val value : 'a t -> 'a

   val isEmpty : 'a p UnPr.t

   val length : 'a p -> Int.t

   val hd : 'a p -> 'a
   val tl : 'a p UnOp.t

   val push : 'a p -> 'a Effect.t
   val pop : 'a p -> 'a Option.t

   val peek : 'a p -> 'a Option.t

   val drop : 'a p Effect.t

   val find : 'a UnPr.t -> 'a p -> ('a p, 'a p) Sum.t
   val fold : ('a * 's -> 's) -> 's -> 'a p -> 's

   val toList : 'a p -> 'a List.t

   val filter : 'a UnPr.t -> 'a p UnOp.t

   val appClear : 'a Effect.t -> 'a p UnOp.t

   val insert : 'a BinPr.t -> 'a p -> 'a Effect.t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   (* SML/NJ workaround --> *)

   datatype 'a t = T of 'a * 'a p
   withtype 'a p = 'a t Option.t Ref.t

   fun ptr () = ref NONE
   fun new v = T (v, ptr ())

   fun next (T (_, p)) = p
   fun value (T (v, _)) = v

   fun isEmpty p = isNone (!p)

   fun nonEmpty f p = case !p of NONE => raise Empty | SOME n => f n
   fun hd p = nonEmpty value p
   fun tl p = nonEmpty next p

   fun drop p = p := !(tl p)

   fun push p v = let
      val n = new v
   in
      next n := !p ; p := SOME n
   end

   fun pop p =
       case !p of
          NONE => NONE
        | SOME (T (v, p')) => (p := !p' ; SOME v)

   fun peek p =
       case !p of
          NONE => NONE
        | SOME (T (v, _)) => SOME v

   fun find c p =
       case !p of
          NONE => INL p
        | SOME (T (v, p')) => if c v then INR p else find c p'

   fun fold f s p =
       case !p of
          NONE => s
        | SOME (T (v, p)) => fold f (f (v, s)) p

   fun toList p = rev (fold op :: [] p)

   fun length p = fold (1 <\ op + o #2) 0 p

   fun filter c p =
       case !p of
          NONE => p
        | SOME (T (v, n)) =>
          if c v then filter c n else (p := !n ; filter c p)

   fun appClear ef p =
       case !p of
          NONE => p
        | SOME (T (v, n)) => (ef v : unit ; p := !n ; appClear ef p)

   fun insert lt p v =
       case !p of
          NONE => push p v
        | SOME (T (x, p')) =>
          if lt (x, v) then insert lt p' v else push p v
end

structure HashMap :> sig
   type ('a, 'b) t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, 'b) t
   val size : ('a, 'b) t -> Int.t
   val insert : ('a, 'b) t -> ('a * 'b) Effect.t
   val find : ('a, 'b) t -> 'a -> 'b Option.t
end = struct
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
