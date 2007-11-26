(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This is a fairly thin wrapper on top of the SDL API.  It should be
 * relatively easy to see the correspondence between these specifications
 * and the SDL API.
 *
 * For documentation on the SDL, see, for example, the
 * [http://www.libsdl.org/cgi/docwiki.cgi/ SDL Documentation Wiki].
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

   structure Prop : sig
      include FLAGS where type flags_word = Word32.t
      val SWSURFACE : flags
      val HWSURFACE : flags
      val ASYNCBLIT : flags
      val ANYFORMAT : flags
      val HWPALETTE : flags
      val DOUBLEBUF : flags
      val FULLSCREEN : flags
      val OPENGL : flags
      val OPENGLBLIT : flags
      val RESIZABLE : flags
      val NOFRAME : flags
   end

   structure Rect : sig
      type t = {x : Int.t, y : Int.t, w : Int.t, h : Int.t}
   end

   structure Surface : sig
      type 'a t
      val free : {video : no} t Effect.t
      val updateRect : 'any t -> Rect.t Option.t Effect.t
      val flip : 'any t Effect.t
   end

   structure Color : sig
      type rgb = {r : Word8.t, g : Word8.t, b : Word8.t}
      type rgba = {r : Word8.t, g : Word8.t, b : Word8.t, a : Word8.t}
      type t
      val fromRGB : 'any Surface.t -> rgb -> t
      val fromRGBA : 'any Surface.t -> rgba -> t
   end

   structure Video : sig
      val setMode : {w : Int.t, h : Int.t, bpp : Int.t, props : Prop.flags}
                    -> {video : yes} Surface.t
      val getSurface : {video : yes} Surface.t Thunk.t
   end

   val fillRect : 'any Surface.t -> Color.t -> Rect.t Option.t Effect.t
end
