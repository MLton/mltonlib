(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Ant Colony Optimization for TSP based on original SML code by Eric
 * Rollins.  See
 *
 *   http://eric_rollins.home.mindspring.com/haskellAnt.html
 *
 * for the original code and explanation by Eric Rollins.  This version
 * has been completely rewritten in a more traditional SML-style (avoiding
 * ref-cells and while-loops) and somewhat optimized (avoiding IntInf
 * arithmetic for random number generation, list concatenation, and linear
 * time population count) being roughly twice as fast as the original on a
 * Pentium M laptop.  This version has also been carefully constructed to
 * produce exactly the same results (same path and cost) as the original.
 * Future versions will likely drop compatibility with the original for
 * simplicity.
 *)

(** Program arguments *********************************************************)

structure Arg = struct
   local
      fun arg i d =
          valOf (Int.fromString (List.nth (CommandLine.arguments (), i)))
          handle _ => d
   in
      val seed      = arg 0 1
      val boost     = arg 1 5
      val iter      = arg 2 100
      val numCities = arg 3 200

      val decr = real boost / real iter
   end
end

(** Random number generator ***************************************************)

structure RNG = struct
   local
      open Ran0Gen.RNG
      val invRealRandMaxPlus1 = 1.0 / (real (Word.toIntX maxValue) + 3.0)
   in
      fun new seed =
          ref (make (Word.fromInt seed))

      fun realBound rng upperBound =
          (rng := next (!rng)
         ; invRealRandMaxPlus1 * upperBound
           * real (Word.toIntX (value (!rng) + 0w1)))

      fun intBound rng upperBound =
          floor (realBound rng (real upperBound))
   end
end

(** A simple fixed-size set implementation ************************************)

structure Set = struct
   datatype t = IN of {count : int ref, array : bool array}
   fun new n = IN {count = ref 0, array = Array.array (n, false)}
   fun add (IN {count, array}) item =
       (if Array.sub (array, item)
        then ()
        else count := !count + 1
      ; Array.update (array, item, true))
   fun has (IN {array, ...}) item = Array.sub (array, item)
   fun count (IN {count, ...}) = !count
end

(** Paths *********************************************************************)

structure Path = struct
   fun gen cities pher rng = let
      val used = Set.new (Array2.nCols cities)
      val start = RNG.intBound rng (Array2.nCols cities)
      fun lp path current =
          if Set.count used = Array2.nCols cities then path else let
             val sumWeight =
                 recur (0, 0.0) (fn lp =>
                    fn (c, sumWeight) =>
                       if c < Array2.nCols cities then
                          lp (c+1,
                              if Set.has used c
                              then sumWeight
                              else sumWeight
                                   + Array2.sub (cities, current, c)
                                     * (1.0 + Array2.sub (pher, current, c)))
                       else
                          sumWeight)
             val rndValue = RNG.realBound rng sumWeight
             val next =
                 recur (0, (0, 0.0)) (fn lp =>
                    fn (c, (next, sumWeight)) =>
                       if c < Array2.nCols cities
                          andalso (Set.has used c
                                   orelse sumWeight < rndValue) then
                          lp (c+1,
                              if Set.has used c
                              then (next, sumWeight)
                              else (c,
                                    sumWeight
                                    + Array2.sub (cities, current, c)
                                      * (1.0 + Array2.sub (pher, current, c))))
                       else
                          next)
          in
             Set.add used next
           ; lp (next::path) next
          end
   in
      Set.add used start
    ; lp [start] start
   end

   fun fold f s =
    fn []     => s
     | c0::cs => let
          fun lp s c' =
           fn []    => f (c0, c0, s)
            | [c]   => f (c0, c, f (c, c', s))
            | c::cs => lp (f (c, c', s)) c cs
       in
          lp s c0 cs
       end

   fun length cities =
       fold (fn (r, c, s) => Array2.sub (cities, r, c) + s) 0.0

   val toString = Cvt.L Cvt.D o rev
end

(** Pheromone *****************************************************************)

structure Pher = struct
   fun new numCities =
       Array2.array (numCities, numCities, 0.0)

   fun update pher =
       Path.fold
        (fn (r, c, ()) =>
            Array2.update
             (pher, r, c, Array2.sub (pher, r, c) + real Arg.boost))
        ()

   val evaporate =
       Array2.modify
        Array2.RowMajor
        (fn v => if v >= Arg.decr then v - Arg.decr else 0.0)
end

(** Main program **************************************************************)

val t = Timer.startRealTimer ()

val maxDistance = 100.0

val cities =
    case RNG.new Arg.seed
     of rng =>
        Array2.tabulate
         Array2.RowMajor
         (Arg.numCities, Arg.numCities, fn _ =>
          RNG.realBound rng (maxDistance - 1.0) + 1.0)

fun work seed = let
   val pher = Pher.new Arg.numCities
   val rng = RNG.new seed
in
   recur (Arg.iter, {len = 0.0, path = []}) (fn lp =>
      fn (0, best) => best
       | (n, best) => let
            val path = Path.gen cities pher rng
            val len = Path.length cities path
            val best =
                if len > #len best
                then (Pher.update pher path ; {len = len, path = path})
                else best
         in
            Pher.evaporate pher
          ; lp (n-1, best)
         end)
end

val {len, path} = work 2

val () =
    (printlns [Path.toString path, " : ", Cvt.G len]
   ; printlns [Time.toString (Timer.checkRealTimer t), " seconds"])
