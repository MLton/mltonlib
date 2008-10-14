(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Protocol :> sig
   val skip : (Unit.t, Socket.active) SocketEvents.monad
   val recv : 'a Rep.t -> ('a, Socket.active) SocketEvents.monad
   val send : 'a Rep.t -> 'a -> (Unit.t, Socket.active) SocketEvents.monad

   structure Fingerprint : sig
      eqtype t
      val t : t Rep.t
      val toWord32 : t -> Word32.t
      val make : 'd Rep.t * 'c Rep.t * String.t -> t
   end

   structure Token : sig
      eqtype t
      val t : t Rep.t
      val zero : t
      val next : t UnOp.t
   end

   structure Request : sig
      datatype t =
         CALL of {token : Token.t,
                  fingerprint : Fingerprint.t} (* value *)
      val t : t Rep.t
      val recv : (t, Socket.active) SocketEvents.monad
      val send : t -> (Unit.t, Socket.active) SocketEvents.monad
   end

   structure Reply : sig
      datatype t =
         UNKNOWN of Token.t
       | RESULT of Token.t (* value *)
       | EXN of Token.t (* value *)
      val t : t Rep.t
      val recv : (t, Socket.active) SocketEvents.monad
      val send : t -> (Unit.t, Socket.active) SocketEvents.monad
   end

   structure Version : sig
      eqtype t
      val current : t
      val recv : (t, Socket.active) SocketEvents.monad
      val send : t -> (Unit.t, Socket.active) SocketEvents.monad
   end
end = struct
   open SocketEvents

   fun buffer n = Word8ArraySlice.full (Word8Array.array (n, 0w0))

   val recv1 =
       SocketEvents.recv (buffer Word32.numBytes) >>= (fn data =>
       SocketEvents.recv
        (buffer
          (LargeWord.toInt
            (PackWord32Little.subArr
              (#1 (Word8ArraySlice.base data), 0)))))

   fun recv t =
       case Generic.unpickler t (IOSMonad.fromReader Word8ArraySlice.getItem)
        of unpickle =>
           recv1 >>= (fn data =>
           try (fn () => unpickle data,
                fn (v, s) =>
                   if Word8ArraySlice.isEmpty s
                   then return v
                   else error (Fail "garbage in packet"),
                error))

   val skip = recv1 >>= (fn _ => return ())

   fun send t =
       case Generic.pickle t
        of pickle =>
           fn value =>
              case pickle value
               of data =>
                  SocketEvents.sendArr
                   (case buffer Word32.numBytes
                     of buffer =>
                        (PackWord32Little.update
                          (#1 (Word8ArraySlice.base buffer),
                           0,
                           LargeWord.fromInt (Word8Vector.length data))
                       ; buffer)) >>= (fn () =>
                  SocketEvents.sendVec (Word8VectorSlice.full data))

   structure Fingerprint = struct
      open Word32
      val toWord32 = id
      fun make (dom, cod, name) =
          Generic.typeHash dom +
          Generic.typeHash cod +
          Generic.hash String.t name
   end

   structure Token = struct
      open Word32
      val zero : t = 0w0
      fun next w : t = w+0w1
   end

   structure Request = struct
      datatype t =
         CALL of {token : Token.t,
                  fingerprint : Fingerprint.t} (* value *)

      val t : t Rep.t =
          data' (C1'"CALL"
                    (record (R'"token" Token.t
                          *` R'"fingerprint" Fingerprint.t)))
                (fn CALL {token=t, fingerprint=f} => t & f,
                 fn t & f => CALL {token=t, fingerprint=f})

      val recv = recv t
      val send = send t
   end

   structure Reply = struct
      datatype t =
         UNKNOWN of Token.t
       | RESULT of Token.t (* value *)
       | EXN of Token.t (* value *)

      val t : t Rep.t =
          data' (C1'"UNKNOWN" Token.t
              +` C1'"RESULT" Token.t
              +` C1'"EXN" Token.t)
                (fn UNKNOWN t => INL (INL t)
                  | RESULT t => INL (INR t)
                  | EXN t => INR t,
                 fn INL (INL t) => UNKNOWN t
                  | INL (INR t) => RESULT t
                  | INR t => EXN t)

      val recv = recv t
      val send = send t
   end

   structure Version = struct
      open Word32
      val current =
          Generic.typeHash Request.t + Generic.typeHash Reply.t
      val recv = recv t
      val send = send t
   end
end
