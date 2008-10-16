(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Client :> CLIENT = struct
   open SocketEvents Async Protocol

   exception UnknownProcedure of Fingerprint.t
   exception ProtocolMismatch of Version.t

   fun run xM socket =
       case ref (INL (Fail "impossible"))
        of result =>
           ((when (xM socket))
             (fn x => result := x)
          ; PollLoop.run Handler.runAll
          ; Exn.reflect (!result))

   structure Conn = struct
      datatype handler =
         HANDLER of {token : Token.t,
                     fingerprint : Fingerprint.t,
                     setExn : Exn.t Effect.t,
                     recvCod : (Unit.t, Socket.active) monad}
      datatype t =
         IN of {socket : Socket.active socket,
                token : Token.t Ref.t,
                handlers : handler ResizableArray.t}

      fun close (IN {socket, ...}) =
          Socket.close socket

      fun byName {host, port} =
          case INetSock.TCP.socket ()
           of socket =>
              (INetSock.TCP.setNODELAY (socket, true)
             ; Socket.connect
                (socket,
                 INetSock.toAddr
                  (NetHostDB.addr
                    (valOf (NetHostDB.getByName host)),
                   port))
             ; try (fn () =>
                       run (Version.send Version.current >>= (fn () =>
                            Version.recv >>= (fn version =>
                            if version <> Version.current
                            then error (ProtocolMismatch version)
                            else return ())))
                           socket,
                    fn () =>
                       IN {socket = socket,
                           token = ref Token.zero,
                           handlers = ResizableArray.new ()},
                    fn e =>
                       (Socket.close socket
                      ; raise e)))
   end

   structure Reply = struct
      datatype 'a t =
         IN of (Conn.t, (Exn.t, 'a) Sum.t) Sum.t Ref.t

      fun drop handlers token' = let
         fun lp i =
             if i < ResizableArray.length handlers
             then case ResizableArray.sub (handlers, i)
                   of handler as Conn.HANDLER {token, ...} =>
                      if token = token'
                      then (ResizableArray.update
                             (handlers,
                              i,
                              ResizableArray.sub
                               (handlers,
                                ResizableArray.length handlers - 1))
                          ; ignore (ResizableArray.pop handlers)
                          ; SOME handler)
                      else lp (i+1)
             else NONE
      in
         lp 0
      end

      val recvExn = recv Exn.t

      fun sync (reply as IN result) =
          case !result
           of INR result => Exn.reflect result
            | INL (Conn.IN {socket, handlers, ...}) =>
              (run (Reply.recv >>= (fn reply =>
                    case drop
                          handlers
                          (case reply
                            of Reply.UNKNOWN token => token
                             | Reply.EXN token => token
                             | Reply.RESULT token => token)
                     of NONE =>
                        (case reply
                          of Reply.UNKNOWN _ => return ()
                           | Reply.EXN _ => skip
                           | Reply.RESULT _ => skip)
                      | SOME
                         (Conn.HANDLER {setExn, recvCod, fingerprint, ...}) =>
                        (case reply
                          of Reply.RESULT _ => recvCod
                           | Reply.EXN _ =>
                             recvExn >>= (fn e =>
                             (setExn e
                            ; return ()))
                           | Reply.UNKNOWN _ =>
                             (setExn (UnknownProcedure fingerprint)
                            ; return ()))))
                   socket
             ; sync reply)
   end

   fun declare (signature' as (dom, cod, _)) = let
      val fingerprint = Fingerprint.fromSignature signature'
      val sendDom = send dom
      val recvCod = recv cod
   in
      fn conn as Conn.IN {socket, handlers, token, ...} => fn value => let
            val token' = Token.next (!token)
            val result = ref (INL conn)
         in
            token := token'
          ; run (Request.send
                  (Request.CALL
                    {token = token',
                     fingerprint = fingerprint}) >>= (fn () =>
                 sendDom value))
                socket
          ; ResizableArray.push
             handlers
             (Conn.HANDLER
               {token = token',
                fingerprint = fingerprint,
                setExn = fn e => result := INR (INL e),
                recvCod = recvCod >>= (fn v =>
                          (result := INR (INR v)
                         ; return ()))})
          ; Reply.IN result
         end
   end
end
