(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* XXX consider supporting HaMLet S and possibly Alice ML as evaluators *)

structure SMLBot :> sig
   val run : {host : String.t, port : String.t, pass : String.t,
              nick : String.t, channel : String.t} Effect.t
end = struct
   structure W8V=Word8Vector and W8VS=Word8VectorSlice

   open Async

   fun relTimeout t = let
      val v = IVar.new ()
   in
      PollLoop.relTimeout (t, IVar.fill v) ; IVar.read v
   end

   local
      fun mk toIODesc poll s = let
         val ch = IVar.new ()
         val pollDesc = poll (valOf (OS.IO.pollDesc (toIODesc s)))
      in
         PollLoop.addDesc
            (pollDesc,
             fn _ => (IVar.fill ch () ; PollLoop.remDesc pollDesc))
       ; IVar.read ch
      end
   in
      val sockEvt = mk Socket.ioDesc
   end

   fun mkSender sock = let
      val msgs = Mailbox.new ()
      fun taking () =
          (when (Mailbox.take msgs))
             (fn msg => let
                    val v = String.concatWith " " msg
                    val v = if size v <= 510 then v else substring (v, 0, 510)
                    val v = String.toBytes (v ^ "\r\n")
                 in
                    sending v (W8V.length v)
                 end)
      and sending v =
          fn 0 => waiting ()
           | n => (when (sockEvt OS.IO.pollOut sock))
                     (fn () =>
                         (sending v)
                            (n-getOpt
                                (Socket.sendVecNB
                                    (sock,
                                     W8VS.slice (v, W8V.length v-n, NONE)),
                                 0)))
      and waiting () =
          (when (relTimeout (Time.fromSeconds 1)))
             taking
   in
      taking () ; Mailbox.send msgs
   end

   val maxLines = 10

   fun mkRunner send = let
      fun stripPrefix i s =
          if #"\n" = String.sub (s, i)   andalso
             #"-"  = String.sub (s, i+1) andalso
             #" "  = String.sub (s, i+2)
          then String.extract (s, i+3, NONE)
          else stripPrefix (i+1) s
      val format =
          (fn l => if length l <= maxLines then l else
                   List.take (l, maxLines-1) @ ["..."]) o
          List.filter (negate (String.isPrefix "[" orElse String.isPrefix "-"))
          o String.tokens (eq #"\n") o stripPrefix 0
      val jobs = Mailbox.new ()
      fun taking () =
          (every (Mailbox.take jobs))
             (fn code => let
                    val proc = Unix.execute ("./run-sandboxed-sml.sh", [])
                    val (ins, outs) = Unix.streamsOf proc
                 in
                    TextIO.output (outs, code)
                  ; TextIO.output1 (outs, #";")
                  ; TextIO.closeOut outs
                  ; send (format (TextIO.inputAll ins)) : Unit.t
                  ; TextIO.closeIn ins
                  ; ignore (Unix.reap proc)
                 end)
   in
      taking () ; Mailbox.send jobs
   end

   fun startReceiver sock send nick run = let
      fun parse ss = let
         open Substring
         fun parseArgs args = let
            val (mids, trail) = position " :" args
            val mids = tokens (eq #" ") mids
            val trail = if isEmpty trail then [] else [string (triml 2 trail)]
         in
            map string mids @ trail
         end

         fun parseCmd prefix rest = let
            val (cmd, args) = splitl (notEq #" ") rest
         in
            {prefix = prefix, cmd = string cmd, args = parseArgs args}
         end
      in
         if SOME #":" <> first ss then parseCmd NONE ss else let
            val (prefix, rest) = splitl (notEq #" ") (triml 1 ss)
         in
            parseCmd (SOME (string prefix)) (triml 1 rest)
         end
      end

      val prefix = nick ^ ":"

      fun receiving ("\n"::"\r"::ss) =
          dispatch (parse (Substring.full (concat (rev ss))))
        | receiving ss =
          (when (sockEvt OS.IO.pollIn sock))
             (fn () =>
                 case Socket.recvVecNB (sock, 1) of
                    NONE => receiving ss
                  | SOME bs => receiving (String.fromBytes bs :: ss))

      and dispatch {cmd, args, ...} =
          (case String.toUpper cmd of
              "PING" => send ["PONG", List.last args]
            | "PRIVMSG" => let
                 val m = List.last args
              in
                 if String.isPrefix prefix m
                 then run (String.extract (m, size prefix, NONE))
                 else ()
              end
            | _ => ()
         ; receiving [])
   in
      receiving []
   end

   fun run {host, port, pass, nick, channel = ch} =
       (With.for (With.around INetSock.TCP.socket Socket.close))
          (fn sock => let
                 val send = mkSender sock
                 val run = mkRunner (app (fn l => send ["NOTICE", ch, ":" ^ l]))
              in
                 Socket.connect
                    (sock,
                     INetSock.toAddr
                        (NetHostDB.addr (valOf (NetHostDB.getByName host)),
                         valOf (Int.fromString port)))
               ; app send
                     [["PASS", pass],
                      ["NICK", nick],
                      ["USER", nick, "0", "*", nick],
                      ["JOIN", ch],
                      ["NOTICE", ch,
                       ":Hello, I'm "^nick^". Try writing \""^nick^
                       ": <code>\"."]]
               ; startReceiver sock send nick run
               ; PollLoop.run Handler.runAll
              end)
end
