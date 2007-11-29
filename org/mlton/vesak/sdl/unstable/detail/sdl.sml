(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure SDL :> SDL = struct
   structure Word32Flags = MkWordFlags (Word32)
   structure Word8Flags = MkWordFlags (Word8)

   val op >>& = With.Monad.>>&
   fun withNew size = With.around (fn () => C.new' size) C.discard'
   fun withAlloc alloc = With.around alloc C.free'
   fun withZs mlStr = withAlloc (fn () => ZString.dupML' mlStr)
   fun withArray size length = withAlloc (fn () => C.alloc' size length)
   fun withBuf length = withArray C.S.uchar length
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

   structure Pos = struct type 'a t = {x : 'a, y : 'a} end
   structure Dim = struct type 'a t = {w : 'a, h : 'a} end
   structure Rect = struct type 'a t = {pos : 'a Pos.t, dim : 'a Dim.t} end
   structure RGB = struct type 'a t = {r : 'a, g : 'a, b : 'a} end
   structure RGBA = struct type 'a t = {r : 'a, g : 'a, b : 'a, a : 'a} end

   structure Pixel = struct
      type t = Word32.t

      structure Format = struct
         type t = {alpha : Word8.t,
                   key : t,
                   bits : Word8.t,
                   bytes : Word8.t,
                   mask : t RGBA.t,
                   shift : Word8.t RGBA.t,
                   loss : Word8.t RGBA.t}
      end

      fun fromRGBA ({shift, loss, ...} : Format.t) {r, g, b, a} = let
         open Word32
         fun pack (v, s, l) =
             (Word32.fromWord8 v >> Word.fromWord8 l) << Word.fromWord8 s
      in
         pack (r, #r shift, #r loss) orb
         pack (g, #g shift, #g loss) orb
         pack (b, #b shift, #b loss) orb
         pack (a, #a shift, #a loss)
      end
      fun fromRGB format {r, g, b} =
          fromRGBA format {r=r, g=g, b=b, a=0w255}
   end

   structure Surface = struct
      type 'a t = (T_SDL_Surface.t, C.rw) C.obj C.ptr'
      fun pixelFormat surface = let
         val pf = C.Ptr.|*! (C.Get.ptr' (S_SDL_Surface.f_format' (C.Ptr.|*! surface)))
         fun w f = C.Get.uint' (f pf)
         fun b f = C.Get.uchar' (f pf)
         open S_SDL_PixelFormat
      in
         {alpha = b f_alpha',
          key   = w f_colorkey',
          bits  = b f_BitsPerPixel',
          bytes = b f_BytesPerPixel',
          mask  = {r=w f_Rmask',  g=w f_Gmask',  b=w f_Bmask',  a=w f_Amask'},
          shift = {r=b f_Rshift', g=b f_Gshift', b=b f_Bshift', a=b f_Ashift'},
          loss  = {r=b f_Rloss',  g=b f_Gloss',  b=b f_Bloss',  a=b f_Aloss'}}
      end
      fun props s = C.Get.uint' (S_SDL_Surface.f_flags' (C.Ptr.|*! s))
      fun dim s = {w = C.Get.sint' (S_SDL_Surface.f_w' (C.Ptr.|*! s)),
                   h = C.Get.sint' (S_SDL_Surface.f_h' (C.Ptr.|*! s))}
      val free = F_SDL_FreeSurface.f'
      val flip = checkInt o F_SDL_Flip.f'
      fun update surface = F_SDL_UpdateRect.f' (surface, 0, 0, 0w0, 0w0)
      fun updateRect surface {pos = {x, y}, dim = {w, h}} =
          F_SDL_UpdateRect.f' (surface, x, y, Word.fromInt w, Word.fromInt h)
      fun fill surface pixel =
          checkInt (F_SDL_FillRect.f' (surface, C.Ptr.null', pixel))
      fun fillRect surface pixel {pos = {x, y}, dim = {w, h}} =
          checkInt (F_SML_SDL_FillRect.f'
                       (surface, x, y, Word.fromInt w, Word.fromInt h, pixel))
      fun blit src dst =
          checkInt (F_SDL_UpperBlit.f' (src, C.Ptr.null', dst, C.Ptr.null'))
      fun blitRect src {pos = {x = sx, y = sy}, dim = {w = sw, h = sh}}
                   dst {pos = {x = dx, y = dy}, dim = {w = dw, h = dh}} =
          checkInt (F_SML_SDL_BlitRect.f'
                       (src, sx, sy, Word.fromInt sw, Word.fromInt sh,
                        dst, dx, dy, Word.fromInt dw, Word.fromInt dh))
      fun convert ({alpha, key, bits, bytes, mask, shift, loss} : Pixel.Format.t)
                  flags surface =
          one (withNew S_SDL_PixelFormat.size)
              (fn pf => let
                     fun w f v = C.Set.uint' (f pf, v)
                     fun b f v = C.Set.uchar' (f pf, v)
                     open S_SDL_PixelFormat
                  in
                     C.Set.ptr' (f_palette' pf, C.Ptr.null')
                   ; b f_alpha' alpha
                   ; w f_colorkey' key
                   ; b f_BitsPerPixel' bits
                   ; b f_BytesPerPixel' bytes
                   ; b f_Rloss' (#r loss) ; b f_Gloss' (#g loss)
                   ; b f_Bloss' (#b loss) ; b f_Aloss' (#a loss)
                   ; w f_Rmask' (#r mask) ; w f_Gmask' (#g mask)
                   ; w f_Bmask' (#b mask) ; w f_Amask' (#a mask)
                   ; b f_Rshift' (#r shift) ; b f_Gshift' (#g shift)
                   ; b f_Bshift' (#b shift) ; b f_Ashift' (#a shift)
                   ; checkPtr (F_SDL_ConvertSurface.f'
                                  (surface, C.Ptr.|&! pf, flags))
                  end)
      fun getClipRect surface =
          one (withNew S_SDL_Rect.size)
              (fn r =>
                  (F_SDL_GetClipRect.f' (surface, C.Ptr.|&! r)
                 ; {pos = {x = Int16.toInt (C.Get.sshort' (S_SDL_Rect.f_x' r)),
                           y = Int16.toInt (C.Get.sshort' (S_SDL_Rect.f_y' r))},
                    dim = {w = Word16.toInt (C.Get.ushort' (S_SDL_Rect.f_w' r)),
                           h = Word16.toInt (C.Get.ushort' (S_SDL_Rect.f_h' r))}}))
      fun setClipRect surface {pos = {x, y}, dim = {w, h}} =
          F_SML_SDL_SetClipRect.f'
             (surface, x, y, Word.fromInt w, Word.fromInt h)
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
      val toFloat = Real32.fromLarge IEEEReal.TO_NEAREST
      fun setGamma {r, g, b} =
          checkInt (F_SDL_SetGamma.f' (toFloat r, toFloat g, toFloat b))
   end

   structure Key = struct
      structure Code = Word8
      structure Sym = struct
         fun toString sym = ZString.toML' (checkPtr (F_SDL_GetKeyName.f' sym))
         open SDLKeySym
      end
      structure Mod = struct
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
      val setRepeat =
       fn NONE => checkInt (F_SDL_EnableKeyRepeat.f' (0, 0))
        | SOME {delay, interval} =>
          checkInt (F_SDL_EnableKeyRepeat.f'
                       (Int.fromLarge (Time.toMilliseconds delay),
                        Int.fromLarge (Time.toMilliseconds interval)))
      val keys = checkPtr (F_SDL_GetKeyState.f' C.Ptr.null')
      fun isPressed sym =
          C.Get.uchar' (C.Ptr.sub' C.S.uchar (keys, E_'SDLKey.m2i sym)) <> 0w0
   end

   structure Mouse = struct
      structure Button = struct
         open Word8Flags
         val LEFT = Word8.fromLargeInt (SDL_BUTTON SDL_BUTTON_LEFT)
         val MIDDLE = Word8.fromLargeInt (SDL_BUTTON SDL_BUTTON_MIDDLE)
         val RIGHT = Word8.fromLargeInt (SDL_BUTTON SDL_BUTTON_RIGHT)
         val WHEELDOWN = Word8.fromLargeInt (SDL_BUTTON SDL_BUTTON_WHEELDOWN)
         val WHEELUP = Word8.fromLargeInt (SDL_BUTTON SDL_BUTTON_WHEELUP)
      end

      local
         fun getMouse f =
             one (withArray C.S.sint 0w2)
                 (fn xy =>
                     (ignore (f (xy, C.Ptr.|+! C.S.sint (xy, 1)))
                    ; {x = C.Get.sint' (C.Ptr.|*! xy),
                       y = C.Get.sint' (C.Ptr.sub' C.S.sint (xy, 1))}))
      in
         fun getPos () = getMouse F_SDL_GetMouseState.f'
         fun getDelta () = getMouse F_SDL_GetRelativeMouseState.f'
      end
      fun getButtons () = F_SDL_GetMouseState.f' (C.Ptr.null', C.Ptr.null')
      fun showCursor b =
          ignore (F_SDL_ShowCursor.f' (if b
                                       then Int.fromLarge SDL_ENABLE
                                       else Int.fromLarge SDL_DISABLE))
   end

   structure Event = struct
      datatype t =
         KEY of {down : Bool.t,
                 pressed : Bool.t,
                 code : Key.Code.t,
                 sym : Key.Sym.t,
                 mods : Key.Mod.flags}

      fun toML event = let
         val t = C.Get.uchar' (U_SDL_Event.f_type' event)
         open E_SDL_Events
         fun is e = m2i e = MLRep.Char.Unsigned.toInt t
      in
         if is e_SDL_KEYDOWN orelse is e_SDL_KEYUP
         then let
               val ke = U_SDL_Event.f_key' event
               val ks = S_SDL_KeyboardEvent.f_keysym' ke
               open S_SDL_keysym
            in
               SOME (KEY {down = is e_SDL_KEYDOWN,
                          pressed = Word8.fromLargeInt SDL_PRESSED =
                                    C.Get.uchar'
                                       (S_SDL_KeyboardEvent.f_state' ke),
                          code = C.Get.uchar' (f_scancode' ks),
                          sym = C.Get.enum' (f_sym' ks),
                          mods = Key.Mod.toML (C.Get.enum' (f_mod' ks))})
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

      val pump = F_SDL_PumpEvents.f'
   end

   structure Image = struct
      fun loadBMP path =
          one (withZs path >>& withZs "rb")
              (fn path & rb =>
                  checkPtr (F_SDL_LoadBMP_RW.f'
                               (F_SDL_RWFromFile.f' (path, rb), 1)))
      fun saveBMP surface path =
          one (withZs path >>& withZs "wb")
              (fn path & wb =>
                  (checkInt (F_SDL_SaveBMP_RW.f'
                                (surface, F_SDL_RWFromFile.f' (path, wb), 1))))
   end
end
