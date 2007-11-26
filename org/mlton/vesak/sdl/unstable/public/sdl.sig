(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is a thin wrapper on top of the SDL API.  It should be easy to see
 * the correspondence between these specifications and the SDL API.  The
 * most visible difference is that instead of returning errors, we raise
 * exceptions.  See, for example, [http://www.libsdl.org/cgi/docwiki.cgi/
 * SDL Documentation Wiki] for documentation on the SDL.
 *)
signature SDL = sig
   structure Init : sig
      include FLAGS where type flags_word = Word32.t
      val TIMER : flags
      val AUDIO : flags
      val VIDEO : flags
      val CDROM : flags
      val JOYSTICK : flags
      val NOPARACHUTE : flags
      val EVENTTHREAD : flags
      val EVERYTHING : flags
   end
   val init : Init.flags Effect.t
   val initSubSystem : Init.flags Effect.t
   val quitSubSystem : Init.flags Effect.t
   val wasInit : Init.flags -> Init.flags
   val quit : Unit.t Effect.t
end
