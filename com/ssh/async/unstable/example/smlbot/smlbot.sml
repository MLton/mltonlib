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
      val iodEvt = mk id
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

   fun startSession send = let
      open TextPrimIO Substring
      val proc = Unix.execute ("./run-sandboxed-sml.sh", [])
      val (ins, outs) = Unix.streamsOf proc
      val (rd, inp) = TextIO.getReader ins

      val die = IVar.new ()

      val rdDesc = RD.ioDesc rd
      fun reading prefix =
          (println "reading"
         ; any [IVar.read die,
                on (iodEvt OS.IO.pollIn rdDesc)
                   (fn () =>
                       case RD.readVecNB rd (RD.chunkSize rd) of
                          NONE => reading prefix
                        | SOME suffix =>
                          if "" = suffix then IVar.fill die () else
                          processLines (full (prefix ^ suffix)))])
      and processLines inp = let
         val (line, rest) = splitl (notEq #"\n") inp
      in
         if isEmpty rest then
            reading (string inp)
         else
            (send (string line) : Unit.t ; processLines (triml 1 rest))
      end

      val wr = #1 (TextIO.getWriter outs)
      val wrDesc = WR.ioDesc wr
      val lines = Mailbox.new ()
      fun waitingLines () =
          (println "waitingLines"
         ; any [IVar.read die,
                on (Mailbox.take lines)
                   (fn line =>
                       writingLine (full (line ^ "\n")))])
      and writingLine line =
          (println "writingLine"
         ; if isEmpty line then waitingLines () else
           any [IVar.read die,
                on (iodEvt OS.IO.pollOut wrDesc)
                   (fn () =>
                       case WR.writeVecNB wr line of
                          NONE => writingLine line
                        | SOME n => writingLine (triml n line))])
   in
      when (IVar.read die)
           (fn () => (print "Closing session... "
                    ; WR.close wr
                    ; RD.close rd
                    ; ignore (Unix.reap proc)
                    ; println "done"))
    ; waitingLines () ; processLines (full inp)
    ; {die = die, run = Mailbox.send lines}
   end

   fun startReceiver sock send nick ch = let
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
      val reset = prefix ^ " (*) reset"

      fun start () = startSession (fn l => send ["NOTICE", ch, ":" ^ l])

      fun receiving (session as {die, ...}) =
       fn "\n"::"\r"::ss =>
          dispatch session (parse (Substring.full (concat (rev ss))))
        | ss =>
          (println "receiving"
         ; any [on (IVar.read die)
                   (fn () =>
                       receiving (start ()) ss),
                on (sockEvt OS.IO.pollIn sock)
                   (fn () =>
                       case Socket.recvVecNB (sock, 1) of
                          NONE => receiving session ss
                        | SOME bs =>
                          if 0 = W8V.length bs then
                             IVar.fill die ()
                          else
                             receiving session (String.fromBytes bs :: ss))])

      and dispatch (session as {run, die}) {cmd, args, ...} =
          (println "dispatch"
         ; case String.toUpper cmd of
              "PING" => (send ["PONG", List.last args] ; receiving session [])
            | "PRIVMSG" => let
                 val m = List.last args
              in
                 if reset = m then
                    (IVar.fill die ()
                   ; receiving (start ()) [])
                 else if String.isPrefix prefix m then
                    (run (String.extract (m, size prefix, NONE))
                   ; receiving session [])
                 else
                    receiving session []
              end
            | _ => receiving session [])
   in
      receiving (start ()) []
   end

   fun run {host, port, pass, nick, channel = ch} =
       (With.for (With.around INetSock.TCP.socket Socket.close))
          (fn sock => let
                 val send = mkSender sock
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
               ; startReceiver sock send nick ch
               ; PollLoop.run Handler.runAll
              end)
end
