(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is an implementation of the N-Body toy benchmark, from the
 * ``Computer Language Benchmarks Game'' (TheGame).
 *
 * In this version, 3D vector arithmetic used in the simulation is
 * implemented using a reusable library rather than manually inlined and
 * specialized code.  The representation of the system has also been
 * simplified to use a list of records instead of multiple arrays.  These
 * changes significantly reduce the amount of code required to write the
 * simulation code and make it significantly more readable.  Nevertheless,
 * the run-time performance of this version is essentially the same as
 * (actually slightly better than) in the SML version used in TheGame at
 * the time of writing.
 *
 * Note that the version currently used in TheGame was originally
 * translated to SML by Matthias Blume who probably tweaked the code for
 * SML/NJ.  In particular, I believe that the reason behind using multiple
 * arrays is to be able to efficiently mutate the position and velocity
 * vectors used in the simulation.
 *)

open Cvt Vec3D

val solarMass = 4.0 * Math.pi * Math.pi
val daysPerYear = 365.24

type body = {pos : Vec3D.t Ref.t, vel : Vec3D.t Ref.t, mass : Real.t}

fun pos (b : body) = ! (#pos b)
fun vel (b : body) = ! (#vel b)

val system =
    map (fn {pos, vel, mass} =>
            {pos = ref pos,
             vel = ref (vel |* daysPerYear),
             mass = mass * solarMass})
        [{pos = {x = 0.0, y = 0.0, z = 0.0},
          vel = {x = 0.0, y = 0.0, z = 0.0},
          mass = 1.0},
         {pos = {x = 4.84143144246472090,
                 y = ~1.16032004402742839,
                 z = ~1.03622044471123109e~1},
          vel = {x = 1.66007664274403694e~3,
                 y = 7.69901118419740425e~3,
                 z = ~6.90460016972063023e~5},
          mass = 9.54791938424326609e~4},
         {pos = {x = 8.34336671824457987,
                 y = 4.12479856412430479,
                 z = ~4.03523417114321381e~1},
          vel = {x = ~2.76742510726862411e~3,
                 y = 4.99852801234917238e~3,
                 z = 2.30417297573763929e~5},
          mass = 2.85885980666130812e~4},
         {pos = {x = 1.28943695621391310e1,
                 y = ~1.51111514016986312e1,
                 z = ~2.23307578892655734e~1},
          vel = {x = 2.96460137564761618e~3,
                 y = 2.37847173959480950e~3,
                 z = ~2.96589568540237556e~5},
          mass = 4.36624404335156298e~5},
         {pos = {x = 1.53796971148509165e1,
                 y = ~2.59193146099879641e1,
                 z = 1.79258772950371181e~1},
          vel = {x = 2.68067772490389322e~3,
                 y = 1.62824170038242295e~3,
                 z = ~9.51592254519715870e~5},
          mass = 5.15138902046611451e~5}]

fun advance dt =
 fn []    => ()
  | a::bs =>
    (app (fn b => let
                val d = pos a |-| pos b
                val l = mag d
                val m = dt / (l * l * l)
             in
                #vel a := vel a |-| d |* #mass b * m
              ; #vel b := vel b |+| d |* #mass a * m
             end)
         bs
   ; #pos a := pos a |+| vel a |* dt
   ; advance dt bs)

val offsetMomentum =
 fn []           => fail "Empty system"
  | sun::planets =>
    #vel sun := foldl (fn (b, v) => v |-| vel b |* #mass b)
                      {x = 0.0, y = 0.0, z = 0.0}
                      planets |/ solarMass

fun energy e =
 fn []    => e
  | a::bs =>
    energy (foldl (fn (b, e) => e - #mass a * #mass b / mag (pos a |-| pos b))
                  (e + 0.5 * #mass a * norm (vel a))
                  bs)
           bs

val n = valOf (Int.fromString (hd (CommandLine.arguments ()))) handle _ => 1000

val () = (offsetMomentum system
        ; println (R'#F 9 (energy 0.0 system))
        ; repeat (fn () => advance 0.01 system) n ()
        ; println (R'#F 9 (energy 0.0 system)))
