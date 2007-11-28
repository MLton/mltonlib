(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure SDL :> SDL = struct
   structure Word32Flags = MkWordFlags (Word32)

   fun withNew size = With.around (fn () => C.new' size) C.discard'
   fun withAlloc alloc = With.around alloc C.free'
   fun withBuf length = withAlloc (fn () => C.alloc' C.S.uchar length)
   val one = With.one

   fun raiseError () = raise Fail (ZString.toML' (F_SDL_GetError.f' ()))
   fun checkInt code = if 0 = code then () else raiseError ()
   fun checkPtr ptr = if C.Ptr.isNull' ptr then raiseError () else ptr

   val minus1ptr : C.voidptr = C.U.i2p (C.Cvt.c_ulong (~ 0w1))

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

   type xy = {x : Int.t, y : Int.t}
   type wh = {w : Int.t, h : Int.t}
   type xywh = {x : Int.t, y : Int.t, w : Int.t, h : Int.t}
   type rgb = {r : Word8.t, g : Word8.t, b : Word8.t}
   type rgba = {r : Word8.t, g : Word8.t, b : Word8.t, a : Word8.t}

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
      type t = Word32.t
      fun fromRGB surface {r, g, b} =
          F_SDL_MapRGB.f' (Surface.getPixelFormat surface, r, g, b)
      fun fromRGBA surface {r, g, b, a} =
          F_SDL_MapRGBA.f' (Surface.getPixelFormat surface, r, g, b, a)
   end

   structure Video = struct
      fun setMode props {bpp} {w, h} =
          checkPtr (F_SDL_SetVideoMode.f' (w, h, bpp, props))
      val getSurface = checkPtr o F_SDL_GetVideoSurface.f'
      val maxDriverNameSz = 256 (* XXX is this large enough? *)
      fun getDriverName () =
         one (withBuf (Word.fromInt maxDriverNameSz))
             (fn buf =>
                 if C.Ptr.isNull' (F_SDL_VideoDriverName.f'
                                      (buf, maxDriverNameSz))
                 then fail "Cannot get driver name.  Is SDL video initialized?"
                 else ZString.toML' buf)
      fun listModes props =
          case F_SDL_ListModes.f' (C.Ptr.null', props)
           of modes =>
              if C.Ptr.isNull' modes then SOME []
              else if minus1ptr = C.Ptr.inject' modes then NONE
              else recur (modes, []) (fn lp =>
                      fn (modes, ms) =>
                         if C.Ptr.isNull' (C.Get.ptr' (C.Ptr.|*! modes))
                         then SOME ms
                         else let
                               val r = C.Ptr.|*! (C.Get.ptr' (C.Ptr.|*! modes))
                               fun `f = Word16.toInt (C.Get.ushort' (f r))
                            in
                               lp (C.Ptr.|+! C.S.ptr (modes, 1),
                                   {w = `S_SDL_Rect.f_w',
                                    h = `S_SDL_Rect.f_h'}::ms)
                            end)
   end

   fun fillRect surface color =
    fn NONE => checkInt (F_SDL_FillRect.f' (surface, C.Ptr.null', color))
     | SOME {x, y, w, h} =>
       checkInt (F_SML_SDL_FillRect.f'
                    (surface, x, y, Word.fromInt w, Word.fromInt h, color))

   structure ScanCode = Word8

   structure Key = SDLKey

   structure Alt = struct
      open Word32Flags
      local
         open E_'SDLMod
      in
         val toML = Word32.fromInt o E_'SDLMod.m2i

         val LSHIFT = toML e_KMOD_LSHIFT
         val RSHIFT = toML e_KMOD_RSHIFT
         val LCTRL  = toML e_KMOD_LCTRL
         val RCTRL  = toML e_KMOD_RCTRL
         val LALT   = toML e_KMOD_LALT
         val RALT   = toML e_KMOD_RALT
         val LMETA  = toML e_KMOD_LMETA
         val RMETA  = toML e_KMOD_RMETA
         val NUM    = toML e_KMOD_NUM
         val CAPS   = toML e_KMOD_CAPS
         val MODE   = toML e_KMOD_MODE
      end
   end

   structure Event = struct
      datatype t =
         KEY of {down : Bool.t,
                 pressed : Bool.t,
                 code : ScanCode.t,
                 key : Key.t,
                 alt : Alt.flags}

      fun toML event = let
         val t = C.Get.uchar' (U_SDL_Event.f_type' event)
         open E_SDL_Events
         fun is e = m2i e = MLRep.Char.Unsigned.toInt t
      in
         if is e_SDL_KEYDOWN orelse is e_SDL_KEYUP
         then let
               val ke = U_SDL_Event.f_key' event
               val ks = S_SDL_KeyboardEvent.f_keysym' ke
            in
               SOME (KEY {down = is e_SDL_KEYDOWN,
                          pressed = Word8.fromLargeInt SDL_PRESSED =
                                    C.Get.uchar' (S_SDL_KeyboardEvent.f_state' ke),
                          code = C.Get.uchar' (S_SDL_keysym.f_scancode' ks),
                          key = C.Get.enum' (S_SDL_keysym.f_sym' ks),
                          alt = Alt.toML (C.Get.enum' (S_SDL_keysym.f_mod' ks))})
            end
         else NONE (* We just ignore other events for now *)
      end

      fun poll () =
          one (withNew U_SDL_Event.size)
              (fn event =>
                  case F_SDL_PollEvent.f' (C.Ptr.|&! event)
                   of 0 => NONE
                    | 1 => toML event
                    | _ => raiseError ())

      fun wait () =
          one (withNew U_SDL_Event.size)
              (fn event =>
                  case F_SDL_WaitEvent.f' (C.Ptr.|&! event)
                   of 1 => (case toML event
                             of NONE => wait ()
                              | SOME e => e)
                    | _ => raiseError ())
   end
end
