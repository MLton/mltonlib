(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Word32Flags = MkWordFlags (Word32)

structure SDL :> SDL = struct
   fun raiseError () = raise Fail (ZString.toML' (F_SDL_GetError.f' ()))
   fun checkInt code = if 0 = code then () else raiseError ()
   fun checkPtr ptr = if C.Ptr.isNull' ptr then raiseError () else ptr

   structure Init = struct
      open Word32Flags
      val ` = Word32.fromLargeInt
      val TIMER       = `SDL_INIT_TIMER
      val AUDIO       = `SDL_INIT_AUDIO
      val VIDEO       = `SDL_INIT_VIDEO
      val CDROM       = `SDL_INIT_CDROM
      val JOYSTICK    = `SDL_INIT_JOYSTICK
      val NOPARACHUTE = `SDL_INIT_NOPARACHUTE
      val EVENTTHREAD = `SDL_INIT_EVENTTHREAD
      val EVERYTHING  = `SDL_INIT_EVERYTHING
   end
   val init = checkInt o F_SDL_Init.f'
   val initSubSystem = checkInt o F_SDL_InitSubSystem.f'
   val quitSubSystem = F_SDL_QuitSubSystem.f'
   val wasInit = F_SDL_WasInit.f'
   val quit = F_SDL_Quit.f'

   structure Prop = struct
      open Word32Flags
      val ` = Word32.fromLargeInt
      val SWSURFACE  = `SDL_SWSURFACE
      val HWSURFACE  = `SDL_HWSURFACE
      val ASYNCBLIT  = `SDL_ASYNCBLIT
      val ANYFORMAT  = `SDL_ANYFORMAT
      val HWPALETTE  = `SDL_HWPALETTE
      val DOUBLEBUF  = `SDL_DOUBLEBUF
      val FULLSCREEN = `SDL_FULLSCREEN
      val OPENGL     = `SDL_OPENGL
      val OPENGLBLIT = `SDL_OPENGLBLIT
      val RESIZABLE  = `SDL_RESIZABLE
      val NOFRAME    = `SDL_NOFRAME
   end

   structure Rect = struct
      type t = {x : Int.t, y : Int.t, w : Int.t, h : Int.t}
   end

   structure Surface = struct
      type 'a t = (T_SDL_Surface.t, C.rw) C.obj C.ptr'
      val free = F_SDL_FreeSurface.f'
      fun updateRect surface =
          F_SDL_UpdateRect.f' o
          (fn NONE => (surface, 0, 0, 0w0, 0w0)
            | SOME {x, y, w, h} => (surface, x, y, Word.fromInt w, Word.fromInt h))
      val flip = checkInt o F_SDL_Flip.f'
      fun getPixelFormat surface =
          C.Get.ptr' (S_SDL_Surface.f_format' (C.Ptr.|*! surface))
   end

   structure Color = struct
      type rgb = {r : Word8.t, g : Word8.t, b : Word8.t}
      type rgba = {r : Word8.t, g : Word8.t, b : Word8.t, a : Word8.t}
      type t = Word32.t
      fun fromRGB surface {r, g, b} =
          F_SDL_MapRGB.f' (Surface.getPixelFormat surface, r, g, b)
      fun fromRGBA surface {r, g, b, a} =
          F_SDL_MapRGBA.f' (Surface.getPixelFormat surface, r, g, b, a)
   end

   structure Video = struct
      fun setMode {w, h, bpp, props} =
          checkPtr (F_SDL_SetVideoMode.f' (w, h, bpp, props))
      val getSurface = checkPtr o F_SDL_GetVideoSurface.f'
   end

   fun fillRect surface color =
    fn NONE => checkInt (F_SDL_FillRect.f' (surface, C.Ptr.null', color))
     | SOME {x, y, w, h} =>
       checkInt (F_SML_SDL_FillRect.f'
                    (surface, x, y, Word.fromInt w, Word.fromInt h, color))
end
