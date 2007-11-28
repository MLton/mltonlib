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

   type xy = {x : Int.t, y : Int.t}
   type wh = {w : Int.t, h : Int.t}
   type xywh = {x : Int.t, y : Int.t, w : Int.t, h : Int.t}
   type rgb = {r : Word8.t, g : Word8.t, b : Word8.t}
   type rgba = {r : Word8.t, g : Word8.t, b : Word8.t, a : Word8.t}

   structure Surface : sig
      type 'a t
      val free : {video : no} t Effect.t
      val updateRect : 'any t -> xywh Option.t Effect.t
      val flip : 'any t Effect.t
   end

   structure Color : sig
      type t
      val fromRGB : 'any Surface.t -> rgb -> t
      val fromRGBA : 'any Surface.t -> rgba -> t
   end

   structure Video : sig
      val setMode : Prop.flags -> {bpp : Int.t} -> wh -> {video : yes} Surface.t
      val getSurface : {video : yes} Surface.t Thunk.t
      val getDriverName : String.t Thunk.t
      val listModes : Prop.flags -> wh List.t Option.t
   end

   val fillRect : 'any Surface.t -> Color.t -> xywh Option.t Effect.t

   structure ScanCode : sig
      eqtype t
   end

   structure Key : SDL_KEY

   structure Alt : sig
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

   structure Event : sig
      datatype t =
         KEY of {down : Bool.t,
                 pressed : Bool.t,
                 code : ScanCode.t,
                 key : Key.t,
                 alt : Alt.flags}
      val poll : t Option.t Thunk.t
      val wait : t Thunk.t
   end
end
