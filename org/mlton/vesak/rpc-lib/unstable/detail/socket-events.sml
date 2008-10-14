(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure SocketEvents :> sig
   exception Closed

   type 'm socket = 'm INetSock.stream_sock

   type ('a, 'm) monad = 'm socket -> (Exn.t, 'a) Sum.t Async.Event.t
   val return : 'a -> ('a, 'm) monad
   val >>= : ('a, 'm) monad * ('a -> ('b, 'm) monad) -> ('b, 'm) monad

   val error : Exn.t -> ('a, 'm) monad

   val sockEvt : OS.IO.poll_desc UnOp.t -> ('m socket, 'm) monad

   val recv : Word8ArraySlice.t -> (Word8ArraySlice.t, Socket.active) monad

   val sendArr : Word8ArraySlice.t -> (Unit.t, Socket.active) monad
   val sendVec : Word8VectorSlice.t -> (Unit.t, Socket.active) monad
end = struct
   open PollLoop Async

   exception Closed

   fun withFill ef =
       case IVar.new ()
        of result => (ef (IVar.fill result) : Unit.t ; IVar.read result)

   type 'm socket = 'm INetSock.stream_sock
   type ('a, 'm) monad = 'm socket -> (Exn.t, 'a) Sum.t Async.Event.t
   fun error e _ = withFill (pass (INL e))
   fun return x _ = withFill (pass (INR x))
   fun (xM >>= x2yM) socket =
       withFill (fn fill =>
        (when (xM socket))
         (fn INL e => fill (INL e)
           | INR x => when (x2yM x socket) fill))

   fun sockEvt poll socket =
       withFill (fn fill =>
        case poll (valOf (OS.IO.pollDesc (Socket.ioDesc socket)))
         of pd => addDesc (pd, fn _ => (fill (INR socket) ; remDesc pd)))

   local
      fun mk isEmpty subslice poll operNB result slice =
          recur slice (fn lp =>
             fn slice =>
                if isEmpty slice
                then return result
                else sockEvt poll >>= (fn socket =>
                     case operNB (socket, slice)
                      of NONE   => error (Fail "impossible")
                       | SOME 0 => error Closed
                       | SOME n =>
                         lp (subslice (slice, n, NONE))))
   in
      fun recv slice =
          mk Word8ArraySlice.isEmpty Word8ArraySlice.subslice
             OS.IO.pollIn Socket.recvArrNB slice slice
      fun sendArr slice =
          mk Word8ArraySlice.isEmpty Word8ArraySlice.subslice
             OS.IO.pollOut Socket.sendArrNB () slice
      fun sendVec slice =
          mk Word8VectorSlice.isEmpty Word8VectorSlice.subslice
             OS.IO.pollOut Socket.sendVecNB () slice
   end
end
