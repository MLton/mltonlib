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
   val wasInit : Init.flags UnOp.t
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

   structure Pos : sig type 'e t = {x : 'e, y : 'e} end
   structure Dim : sig type 'e t = {w : 'e, h : 'e} end
   structure Rect : sig type 'e t = {pos : 'e Pos.t, dim : 'e Dim.t} end
   structure RGB : sig type 'e t = {r : 'e, g : 'e, b : 'e} end
   structure RGBA : sig type 'e t = {r : 'e, g : 'e, b : 'e, a : 'e} end

   structure Pixel : sig
      eqtype t
      structure Format : sig eqtype t end
      val fromRGB : Format.t -> Word8.t RGB.t -> t
      val fromRGBA : Format.t -> Word8.t RGBA.t -> t
   end

   structure Surface : sig
      type 'a t
      val pixelFormat : 'any t -> Pixel.Format.t
      val props : 'any t -> Prop.flags
      val dim : 'any t -> Int.t Dim.t
      val free : {video : no} t Effect.t
      val flip : 'dst t Effect.t
      val update : 'dst t Effect.t
      val updateRect : 'dst t -> Int.t Rect.t Effect.t
      val fill : 'dst t -> Pixel.t Effect.t
      val fillRect : 'dst t -> Pixel.t -> Int.t Rect.t Effect.t
      val blit : 'src t -> 'dst t Effect.t
      val blitRect : 'src t -> Int.t Rect.t -> 'dst t -> Int.t Rect.t Effect.t
      val convert : Pixel.Format.t -> Prop.flags -> 'any t -> {video : no} t
      val getClipRect : 'any t -> Int.t Rect.t
      val setClipRect : 'any t -> Int.t Rect.t Effect.t
   end

   structure Video : sig
      val setMode : Prop.flags -> {bpp : Int.t} -> Int.t Dim.t
                    -> {video : yes} Surface.t
      val getSurface : {video : yes} Surface.t Thunk.t
      val getDriverName : String.t Thunk.t
      val listModes : Prop.flags -> Int.t Dim.t List.t Option.t
      val setGamma : Real.t RGB.t Effect.t
   end

   structure Key : sig
      structure Code : sig eqtype t end
      structure Sym : SDL_KEY_SYM
      structure Mod : sig
         include FLAGS where type flags_word = Word32.t
         val LSHIFT : flags
         val RSHIFT : flags
         val LCTRL : flags
         val RCTRL : flags
         val LALT : flags
         val RALT : flags
         val LMETA : flags
         val RMETA : flags
         val NUM : flags
         val CAPS : flags
         val MODE : flags
      end
      val setRepeat : {delay : Time.t, interval : Time.t} Option.t Effect.t
      val isPressed : Sym.t UnPr.t
   end

   structure Mouse : sig
      structure Button : sig
         include FLAGS where type flags_word = Word8.t
         val LEFT : flags
         val MIDDLE : flags
         val RIGHT : flags
         val WHEELDOWN : flags
         val WHEELUP : flags
      end
      val getPos : Int.t Pos.t Thunk.t
      val getDelta : Int.t Pos.t Thunk.t
      val getButtons : Button.flags Thunk.t
      val showCursor : Bool.t Effect.t
   end

   structure Event : sig
      datatype t =
         KEY of {down : Bool.t,
                 pressed : Bool.t,
                 code : Key.Code.t,
                 sym : Key.Sym.t,
                 mods : Key.Mod.flags}
      val poll : t Option.t Thunk.t
      val wait : t Thunk.t
      val pump : Unit.t Effect.t
   end

   structure Image : sig
      val loadBMP : String.t -> {video : no} Surface.t
      val saveBMP : 'any Surface.t -> String.t Effect.t
   end
end
