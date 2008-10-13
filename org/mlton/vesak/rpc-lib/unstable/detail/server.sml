(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Server :> SERVER = struct
   open SocketEvents Async Protocol

   val entries :
       {fingerprint : Fingerprint.t,
        procedure : Token.t -> Unit.t monad} List.t Ref.t =
       ref []

   fun find fingerprint =
       List.find (eq fingerprint o #fingerprint) (!entries)

   val sendExn = send Exn.t

   fun define (signature' as (dom, cod, _)) = let
      val recvDom = recv dom
      val sendCod = send cod
      open Reply
   in
      fn f =>
         (push entries)
          {fingerprint = Fingerprint.make signature',
           procedure = fn token =>
            recvDom >>= (fn x =>
             try (fn () => f x,
                  fn y =>
                     send (RESULT token) >>= (fn () =>
                     sendCod y),
                  fn e =>
                     send (EXN token) >>= (fn () =>
                     sendExn e)))}
   end

   fun serve () =
       Request.recv >>= (fn req =>
       case req
        of Request.CALL {token = token, fingerprint = fingerprint} =>
           case find fingerprint
            of NONE =>
               skip >>= (fn () =>
               Reply.send (Reply.UNKNOWN token) >>=
               serve)
             | SOME {procedure, ...} =>
               procedure token >>= serve)

   fun run {port, accept=filter} = let
      fun negotiate addr =
          if not (filter addr)
          then error (Fail "addr")
          else Version.recv >>= (fn version' =>
               if version' <> Version.current
               then error (Fail "version")
               else Version.send version' >>= serve)

      fun accept ? =
          (SocketEvents.sockEvt OS.IO.pollIn >>= (fn socket =>
           case Socket.acceptNB socket
            of NONE => error (Fail "NONE")
             | SOME (socket, addr) =>
               (INetSock.TCP.setNODELAY (socket, true)
              ; (when (negotiate addr socket))
                 (fn r =>
                     (Socket.close socket
                    ; case r
                       of INR () => ()
                        | INL e  =>
                          case e
                           of Closed => ()
                            | e =>
                              printlns
                               ("unhandled exception: " ::
                                Exn.message e ::
                                List.intersperse
                                 "\n"
                                 (Exn.history e))))
              ; accept))) ?

      val socket = INetSock.TCP.socket ()
   in
      (Socket.bind
        (socket,
         INetSock.toAddr
          (valOf (NetHostDB.fromString "127.0.0.1"), port))
     ; Socket.listen (socket, 16))
      handle e => (Socket.close socket ; raise e)
    ; (when (accept socket))
       (fn r =>
           (Socket.close socket
          ; case r
             of INL e  => println (Exn.message e)
              | INR () => ()))
    ; PollLoop.run Handler.runAll
   end
end
