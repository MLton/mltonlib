(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Word32Flags = MkWordFlags (Word32)

structure SDL :> SDL = struct
   fun check code =
       if 0 = code
       then ()
       else raise Fail (ZString.toML' (F_SDL_GetError.f' ()))

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
   val init = check o F_SDL_Init.f'
   val initSubSystem = check o F_SDL_InitSubSystem.f'
   val quitSubSystem = F_SDL_QuitSubSystem.f'
   val wasInit = F_SDL_WasInit.f'
   val quit = F_SDL_Quit.f'
end
