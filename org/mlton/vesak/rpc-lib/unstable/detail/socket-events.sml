(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure SocketEvents :> sig
   exception Closed

   type socket = Socket.active INetSock.stream_sock

   include MONAD_CORE
   where type 'a monad = socket -> (Exn.t, 'a) Sum.t Async.Event.t

   val error : Exn.t -> 'a monad

   val sockEvt : OS.IO.poll_desc UnOp.t -> socket monad

   val recv : Word8ArraySlice.t -> Word8ArraySlice.t monad

   val sendArr : Word8ArraySlice.t -> Unit.t monad
   val sendVec : Word8VectorSlice.t -> Unit.t monad
end = struct
   open PollLoop Async

   exception Closed

   type socket = Socket.active INetSock.stream_sock

   type 'a monad = socket -> (Exn.t, 'a) Sum.t Async.Event.t
   fun error e _ =
       case IVar.new ()
        of result => (IVar.fill result (INL e) ; IVar.read result)
   fun return x _ =
       case IVar.new ()
        of result => (IVar.fill result (INR x) ; IVar.read result)
   fun (xM >>= x2yM) socket =
       case IVar.new ()
        of result =>
           ((when (xM socket))
             (fn INL e => IVar.fill result (INL e)
               | INR x =>
                 (when (x2yM x socket))
                  (IVar.fill result))
          ; IVar.read result)

   local
      fun mk toIODesc poll s = let
         val ch = IVar.new ()
         val pollDesc = poll (valOf (OS.IO.pollDesc (toIODesc s)))
      in
         addDesc
          (pollDesc, fn _ => (IVar.fill ch (INR s) ; remDesc pollDesc))
       ; IVar.read ch
      end
   in
      fun sockEvt ? = mk Socket.ioDesc ?
    (*fun iodEvt ? = mk id ?*)
   end

   fun recv fullSlice =
       recur fullSlice (fn lp =>
          fn slice =>
             if Word8ArraySlice.isEmpty slice
             then return fullSlice
             else sockEvt OS.IO.pollIn >>= (fn socket =>
                  case Socket.recvArrNB (socket, slice)
                   of NONE   => error (Fail "impossible")
                    | SOME 0 => error Closed
                    | SOME n =>
                      lp (Word8ArraySlice.subslice (slice, n, NONE))))

   local
      fun mk isEmpty subslice sendNB slice =
          recur slice (fn lp =>
             fn slice =>
                if isEmpty slice
                then return ()
                else sockEvt OS.IO.pollOut >>= (fn socket =>
                     case sendNB (socket, slice)
                      of NONE   => error (Fail "impossible")
                       | SOME 0 => error Closed
                       | SOME n =>
                         lp (subslice (slice, n, NONE))))
   in
      val sendArr =
          mk Word8ArraySlice.isEmpty Word8ArraySlice.subslice Socket.sendArrNB

      val sendVec =
          mk Word8VectorSlice.isEmpty Word8VectorSlice.subslice Socket.sendVecNB
   end
end
