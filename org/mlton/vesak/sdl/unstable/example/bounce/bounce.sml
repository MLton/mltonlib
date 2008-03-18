(* Copyright (C) 2007-2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Iter Cvt

structure Opt = struct
   val w = ref 640
   val h = ref 480
   val fs = ref false
   val bpp = ref NONE
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
   val display =
       SDL.Video.setMode
          (case !Opt.bpp
            of NONE => SDL.Video.getPixelFormat ()
             | SOME 16 => SDL.Pixel.Format.r5g6b5
             | SOME 24 => SDL.Pixel.Format.r8g8b8
             | SOME 32 => SDL.Pixel.Format.r8g8b8a8
             | _ => fail "Unsupported pixel format")
          let open SDL.Prop in
             flags ([DOUBLEBUF] @
                    (if !Opt.fs then [HW, FULLSCREEN] else [])) end
          {w = !Opt.w, h = !Opt.h}

   val format = SDL.Surface.getPixelFormat display

   val chest =
       SDL.Surface.convertToVideo {alpha=false} (SDL.Image.loadBMP "chest.bmp")
   val chestDim as {w = chestW, h = chestH} = SDL.Surface.getDim chest

   val green = SDL.Pixel.fromRGB format {r=0w000, g=0w128, b=0w000}
   val red   = SDL.Pixel.fromRGB format {r=0w128, g=0w000, b=0w000}
   val blue  = SDL.Pixel.fromRGB format {r=0w000, g=0w000, b=0w128}

   val w = !Opt.w
   val h = !Opt.h

   val xMax = real (w - !Opt.size)
   val yMax = real (h - !Opt.size)

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
      (upToBy 0 w chestW >< upToBy 0 h chestH)
       (fn x & y =>
           SDL.Surface.blitRect
            chest {pos = {x=0, y=0}, dim = chestDim}
            display {pos = {x=x, y=y}, dim = chestDim})
    ; (inVector obs)
       (fn {x, y, ...} =>
           SDL.Surface.fillRect
            display
            color
            {dim = obDim, pos = {x = trunc (!x), y = trunc (!y)}})
    ; SDL.Surface.fillRect
         display
         let
            open SDL.Mouse.Button
            val buttons = SDL.Mouse.getButtons ()
         in
            if anySet (LEFT, buttons) then red
            else if anySet (RIGHT, buttons) then green
            else blue
         end
         {dim = obDim, pos = SDL.Mouse.getPos ()}
    ; SDL.Surface.flip display
   end

   fun animate () =
       (inVector obs)
        (fn {x, y, dx, dy} => let
               fun upd (v, dv, vMax) =
                   (if !v < 0.0 andalso !dv < 0.0 orelse
                       vMax < !v andalso 0.0 < !dv
                    then dv := ~ (!dv) else ()
                  ; v := !v + !dv)
            in
               upd (x, dx, xMax)
             ; upd (y, dy, yMax)
            end)

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
 ; SDL.Mouse.setPos {x = 0, y = 0}
 ; SDL.Key.setRepeat NONE
 ; lp ()
end

fun main () =
    (printlns ["Driver name: ", SDL.Video.getDriverName ()]
   ; print "Available full screen modes: "
   ; case SDL.Video.listModes
             (SDL.Video.getPixelFormat ())
             let open SDL.Prop in flags [DOUBLEBUF, HW, FULLSCREEN] end
      of NONE    => println "Any resolution is OK?"
       | SOME [] => println "None"
       | SOME rs => println o String.concatWith ", " |<
                    map (fn {w, h} => concat [D w, "x", D h]) rs
   ; demo ())

val s2i = valOf o Int.fromString

val () =
    recur (CommandLine.arguments ()) (fn lp =>
       fn "-bpp"  :: v :: xs => (Opt.bpp  := SOME (s2i v) ; lp xs)
        | "-w"    :: v :: xs => (Opt.w    := s2i v ; lp xs)
        | "-h"    :: v :: xs => (Opt.h    := s2i v ; lp xs)
        | "-size" :: v :: xs => (Opt.size := s2i v ; lp xs)
        | "-num"  :: v :: xs => (Opt.num  := s2i v ; lp xs)
        | "-fps"  :: v :: xs => (Opt.fps  := s2i v ; lp xs)
        | "-fs"        :: xs => (Opt.fs   := true ; lp xs)
        | x :: _             => (printlns ["Invalid option: ", x])
        | []                 => (SDL.init SDL.Init.VIDEO
                               ; after (main, SDL.quit)))
