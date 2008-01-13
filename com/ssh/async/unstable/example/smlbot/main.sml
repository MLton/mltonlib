(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

val () = let
   val host = ref (SOME "127.0.0.1")
   val port = ref (SOME "6667")
   val pass = ref (SOME "smlbot")
   val nick = ref (SOME "smlbot")
   val channel = ref (SOME "#sml")
   val get = valOf o !
   fun set opt = opt <\ op := o SOME
in
   recur (CommandLine.arguments ()) (fn lp =>
      fn [] =>
         SMLBot.run {host = get host, port = get port,
                     pass = get pass, nick = get nick,
                     channel = get channel}
       | "-help"::_ =>
         print "Usage: smlbot [option ...]\n\
               \\n\
               \Options:\n\
               \  -channel <channel>\n\
               \  -host <host>\n\
               \  -nick <nick>\n\
               \  -pass <pass>\n\
               \  -port <port>\n"
       | opt::arg::rest =>
         (set (case opt of
                  "-host" => host
                | "-port" => port
                | "-pass" => pass
                | "-nick" => nick
                | "-channel" => channel
                | _ => fails ["Invalid option ", opt]) arg
        ; lp rest)
       | opt::_ => fails ["Invalid option ", opt])
end
