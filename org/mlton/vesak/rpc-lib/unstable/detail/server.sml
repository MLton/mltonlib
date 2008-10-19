(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Server :> SERVER = struct
   open SocketEvents Async Protocol

   structure ProcMap = struct
      type t = (Fingerprint.t,
                Token.t -> (Unit.t, Socket.active) monad) HashMap.t

      fun new () =
          HashMap.new
           {eq = (op =),
            hash = Word32.toWord o Fingerprint.toWord32}

      val sendExn = send Exn.t

      fun add entries (signature' as (dom, cod, name)) = let
         val fingerprint = Fingerprint.fromSignature signature'
         val recvDom = recv dom
         val sendCod = send cod
         open Reply
      in
         fn f =>
            case HashMap.find entries fingerprint
             of SOME _ => fails ["fingerprint of ", name, " already in use"]
              | NONE =>
                (HashMap.insert entries)
                 (fingerprint,
                  fn token =>
                     recvDom >>= (fn x =>
                     try (fn () => f x,
                          fn y =>
                             send (RESULT token) >>= (fn () =>
                             sendCod y),
                          fn e =>
                             send (EXN token) >>= (fn () =>
                             sendExn e))))
      end
   end

   structure TCP = struct
      type start_args =
           {name : String.t,
            port : Int.t,
            numAccepts : Int.t Option.t,
            tcpNoDelay : Bool.t,
            serverError : Exn.t Effect.t,
            closed : Unit.t Effect.t,
            accept : {addr : INetSock.sock_addr} UnPr.t,
            protocolMismatch :
            {addr : INetSock.sock_addr,
             version : Protocol.Version.t} Effect.t,
            connected : {addr : INetSock.sock_addr} Effect.t,
            unknownProc :
            {addr : INetSock.sock_addr,
             fingerprint : Protocol.Fingerprint.t} Effect.t,
            protocolError :
            {addr : INetSock.sock_addr, error : Exn.t} Effect.t,
            disconnected : {addr : INetSock.sock_addr} Effect.t}
      type 'a start = ('a, start_args) FRU.upd

      fun start' entries
                 ({name, port, numAccepts, tcpNoDelay, serverError, closed,
                   accept, protocolMismatch, connected, unknownProc,
                   protocolError, disconnected} : start_args) = let
         fun serve addr =
             Request.recv >>= (fn req =>
             case req
              of Request.CALL {token, fingerprint} =>
                 case HashMap.find entries fingerprint
                  of NONE =>
                     (unknownProc {addr = addr, fingerprint = fingerprint}
                    ; skip >>= (fn () =>
                      Reply.send (Reply.UNKNOWN token) >>= (fn () =>
                      serve addr)))
                   | SOME procedure =>
                     procedure token >>= (fn () =>
                     serve addr))

         fun negotiate addr =
             Version.send Version.current >>= (fn () =>
             Version.recv >>= (fn version =>
             if version <> Version.current
             then (protocolMismatch {addr = addr, version = version}
                 ; return ())
             else (connected {addr = addr}
                 ; serve addr)))

         fun listen numAccepts =
             if SOME 0 = numAccepts
             then return ()
             else SocketEvents.sockEvt OS.IO.pollIn >>= (fn socket =>
                  case Socket.acceptNB socket
                   of NONE => error (Fail "acceptNB returned NONE")
                    | SOME (socket, addr) =>
                      (if not (accept {addr = addr})
                       then (Socket.close socket
                           ; listen numAccepts)
                       else (INetSock.TCP.setNODELAY (socket, tcpNoDelay)
                           ; (when (negotiate addr socket))
                              (fn r =>
                                  (Socket.close socket
                                 ; case r
                                    of INR () => ()
                                     | INL Closed => ()
                                     | INL e =>
                                       protocolError {addr = addr, error = e}
                                 ; disconnected {addr = addr}))
                           ; listen (Option.map (fn n => n-1) numAccepts))))

         val socket = INetSock.TCP.socket ()
      in
         (Socket.bind
           (socket,
            INetSock.toAddr
             (NetHostDB.addr
               (valOf (NetHostDB.getByName name)),
              port))
        ; Socket.listen (socket, 16))
         handle e => (Socket.close socket ; raise e)
       ; (when (listen numAccepts socket))
          (fn r =>
              (Socket.close socket
             ; case r
                of INL e  => serverError e
                 | INR () => ()
             ; closed ()))
      end

      val ~ =
          (fn {name=a, port=b, numAccepts=c, tcpNoDelay=d, serverError=e,
               closed=f, accept=g, protocolMismatch=h, connected=i,
               unknownProc=j, protocolError=k, disconnected=l} =>
              (a&b&c&d&e&f&g&h&i&j&k&l),
           fn (a&b&c&d&e&f&g&h&i&j&k&l) =>
              {name=a, port=b, numAccepts=c, tcpNoDelay=d, serverError=e,
               closed=f, accept=g, protocolMismatch=h, connected=i,
               unknownProc=j, protocolError=k, disconnected=l})

      fun start entries =
          let open FRU in args A A A A A A A A A A A A $ ~ ~ end
           {name = "127.0.0.1", port = 45678, numAccepts = NONE,
            tcpNoDelay = false, serverError = ignore, closed = ignore,
            accept = const true, protocolMismatch = ignore, connected = ignore,
            unknownProc = ignore, protocolError = ignore, disconnected = ignore}
           (start' entries)
   end

   fun run () = PollLoop.run Handler.runAll
end
