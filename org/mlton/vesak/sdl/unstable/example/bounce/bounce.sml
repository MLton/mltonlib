(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

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
       SDL.Video.setMode
          let open SDL.Prop in
             flags ([DOUBLEBUF] @
                    (if !Opt.fs then [HWSURFACE, FULLSCREEN] else [])) end
          {bpp = !Opt.bpp}
          {w = !Opt.w, h = !Opt.h}

   val format = SDL.Surface.pixelFormat surface

   val black = SDL.Pixel.fromRGB format {r=0w000, g=0w000, b=0w000}
   val green = SDL.Pixel.fromRGB format {r=0w000, g=0w255, b=0w000}
   val red   = SDL.Pixel.fromRGB format {r=0w255, g=0w000, b=0w000}
   val blue  = SDL.Pixel.fromRGB format {r=0w000, g=0w000, b=0w255}

   val xMax = real (!Opt.w - !Opt.size)
   val yMax = real (!Opt.h - !Opt.size)

   val obs =
       Vector.tabulate
          (!Opt.num,
           fn _ => {x = ref (G.gen (G.realInRange (0.0, xMax))),
                    y = ref (G.gen (G.realInRange (0.0, yMax))),
                    dx = ref (G.gen (G.realInRange (~5.0, 5.0))),
                    dy = ref (G.gen (G.realInRange (~5.0, 5.0)))})

   val obDim = {w = !Opt.size, h = !Opt.size}

   fun render () = let
      val color = if SDL.Key.isPressed SDL.Key.Sym.SPACE then red else green
   in
      SDL.Surface.fill surface black
    ; Vector.app (fn {x, y, ...} =>
                     SDL.Surface.fillRect
                        surface
                        color
                        {dim = obDim,
                         pos = {x = trunc (!x), y = trunc (!y)}}) obs
    ; SDL.Surface.fillRect
         surface
         let
            open SDL.Mouse.Button
            val buttons = SDL.Mouse.getButtons ()
         in
            if anySet (LEFT, buttons) then red
            else if anySet (RIGHT, buttons) then green
            else blue
         end
         {dim = obDim, pos = SDL.Mouse.getPos ()}
    ; SDL.Surface.flip surface
   end

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
       case SDL.Event.poll ()
        of SOME (SDL.Event.KEY {sym, pressed, ...}) =>
           if sym = SDL.Key.Sym.Q orelse
              sym = SDL.Key.Sym.ESCAPE
           then ()
           else (printlns ["Key ", SDL.Key.Sym.toString sym, " ",
                           if pressed then "pressed" else "released"]
               ; lp ())
         | _ => (render () ; animate () ; sleep () ; lp ())
in
   SDL.Mouse.showCursor false
 ; lp ()
end

fun main () =
    (printlns ["Driver name: ", SDL.Video.getDriverName ()]
   ; print "Available full screen modes: "
   ; case SDL.Video.listModes
             let open SDL.Prop in flags [DOUBLEBUF, HWSURFACE, FULLSCREEN] end
      of NONE    => println "Any resolution is OK?"
       | SOME [] => println "None"
       | SOME rs =>
         println o String.concatWith ", " |< map
            (fn {w, h} => concat [Int.toString w, "x", Int.toString h]) rs
   ; demo ())

val () =
    recur (CommandLine.arguments ()) (fn lp =>
       fn "-bpp"  :: v :: xs => (Opt.bpp  := valOf (Int.fromString v) ; lp xs)
        | "-w"    :: v :: xs => (Opt.w    := valOf (Int.fromString v) ; lp xs)
        | "-h"    :: v :: xs => (Opt.h    := valOf (Int.fromString v) ; lp xs)
        | "-size" :: v :: xs => (Opt.size := valOf (Int.fromString v) ; lp xs)
        | "-num"  :: v :: xs => (Opt.num  := valOf (Int.fromString v) ; lp xs)
        | "-fps"  :: v :: xs => (Opt.fps  := valOf (Int.fromString v) ; lp xs)
        | "-fs"        :: xs => (Opt.fs   := true                     ; lp xs)
        | x :: _             => (printlns ["Invalid option: ", x])
        | []                 => (SDL.init SDL.Init.VIDEO
                               ; after (main, SDL.quit)))
