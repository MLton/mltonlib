(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open SDL

val printlns = println o concat

structure Opt = struct
   val w = ref 640
   val h = ref 480
   val fs = ref false
   val bpp = ref 16
   val size = ref 4
   val num = ref 100
   val fps = ref 50
end

structure G = struct
   open RanQD1Gen
   local
      val r = ref (RNG.make (RNG.Seed.fromWord (valOf (RandomDev.useed ()))))
   in
      fun gen g = generate 1 (!r before r := RNG.next (!r)) g
   end
end

fun demo () = let
   val surface =
       Video.setMode
          let open Prop in
             flags ([HWSURFACE] @ (if !Opt.fs then [FULLSCREEN] else [])) end
          {bpp = !Opt.bpp}
          {w = !Opt.w, h = !Opt.h}

   val black = Color.fromRGB surface {r=0w0, g=0w0, b=0w0}
   val white = Color.fromRGB surface {r=0w255, g=0w255, b=0w255}

   val xMax = real (!Opt.w - !Opt.size)
   val yMax = real (!Opt.h - !Opt.size)

   val obs =
       Vector.tabulate
          (!Opt.num,
           fn _ => {x = ref (G.gen (G.realInRange (0.0, xMax))),
                    y = ref (G.gen (G.realInRange (0.0, yMax))),
                    dx = ref (G.gen (G.realInRange (~5.0, 5.0))),
                    dy = ref (G.gen (G.realInRange (~5.0, 5.0)))})

   fun render () =
       (fillRect surface black NONE
      ; Vector.app (fn {x, y, ...} =>
                       fillRect surface white (SOME {x = trunc (!x),
                                                     y = trunc (!y),
                                                     w = !Opt.size,
                                                     h = !Opt.size}))
                   obs
      ; Surface.updateRect surface NONE)

   fun animate () =
       Vector.app (fn {x, y, dx, dy} => let
                         fun upd (v, dv, vMax) =
                             (if !v < 0.0 andalso !dv < 0.0 orelse
                                 vMax < !v andalso 0.0 < !dv
                              then dv := ~ (!dv) else ()
                            ; v := !v + !dv)
                      in
                         upd (x, dx, xMax)
                       ; upd (y, dy, yMax)
                      end)
                  obs

   local
      open Time
      val slice = fromMicroseconds (toMicroseconds (fromSeconds 1) div
                                    Int.toLarge (!Opt.fps))
      val minSleep = fromMicroseconds 1
      val next = ref (now () + slice)
   in
      fun sleep () =
          (OS.Process.sleep (Cmp.max compare (!next - now (), minSleep))
         ; next := !next + slice)
   end

   fun lp () =
       case Event.poll ()
        of SOME (Event.KEY {key, pressed = true, down = true, ...}) =>
           if key = Key.Q orelse key = Key.ESCAPE then () else lp ()
         | _ => (render () ; animate () ; sleep () ; lp ())
in
   lp ()
end

fun main () =
    (printlns ["Driver name: ", Video.getDriverName ()]
   ; print "Available full screen modes: "
   ; case Video.listModes let open Prop in flags [HWSURFACE, FULLSCREEN] end
      of NONE    => println "Any resolution is OK?"
       | SOME [] => println "None"
       | SOME rs =>
         println o String.concatWith ", " |< map
            (fn {w, h} => concat [Int.toString w, "x", Int.toString h]) rs
   ; demo ())

val () =
    recur (CommandLine.arguments ()) (fn lp =>
       fn []                 => (init Init.VIDEO ; after (main, quit))
        | "-bpp"  :: v :: xs => (Opt.bpp  := valOf (Int.fromString v) ; lp xs)
        | "-w"    :: v :: xs => (Opt.w    := valOf (Int.fromString v) ; lp xs)
        | "-h"    :: v :: xs => (Opt.h    := valOf (Int.fromString v) ; lp xs)
        | "-size" :: v :: xs => (Opt.size := valOf (Int.fromString v) ; lp xs)
        | "-num"  :: v :: xs => (Opt.num  := valOf (Int.fromString v) ; lp xs)
        | "-fps"  :: v :: xs => (Opt.fps  := valOf (Int.fromString v) ; lp xs)
        | "-fs"        :: xs => (Opt.fs   := true                     ; lp xs)
        | x :: _             => (printlns ["Invalid option: ", x]))
