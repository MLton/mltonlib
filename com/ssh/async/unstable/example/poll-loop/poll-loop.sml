(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure PollLoop :> sig
   val run : Unit.t Effect.t Effect.t

   val stop : Unit.t Effect.t

   val addDesc : (OS.IO.poll_desc * OS.IO.poll_info Effect.t) Effect.t
   val remDesc : OS.IO.poll_desc Effect.t

   val absTimeout : (Time.t * Unit.t Effect.t) Effect.t
   val relTimeout : (Time.t * Unit.t Effect.t) Effect.t
end = struct
   val doStop = ref false
   fun stop () = doStop := true

   val descs : (OS.IO.poll_desc * OS.IO.poll_info Effect.t) List.t Ref.t =
       ref []
   fun findDesc d k =
       recur ([] & !descs) (fn lp =>
          fn _ & [] => fail "findDesc"
           | fs & e::es => if #1 e = d then k (fs, e, es) else lp (e::fs & es))
   val addDesc = List.push descs
   fun remDesc d =
       findDesc d (fn (fs, _, es) => descs := List.revAppend (fs, es))

   val timeouts : (Time.t * Unit.t Effect.t) List.t Ref.t = ref []
   fun absTimeout (absTime, action) = let
      fun here fs es = timeouts := List.revAppend (fs, es)
   in
      recur ([] & !timeouts) (fn lp =>
         fn fs & [] => here fs [(absTime, action)]
          | fs & e::es => if Time.<= (#1 e, absTime) then lp (e::fs & es)
                          else here fs ((absTime, action)::es))
   end
   fun relTimeout (relTime, action) =
       absTimeout (Time.+ (Time.now (), relTime), action)

   fun run ef =
       (ef () : Unit.t
      ; if null (!descs) orelse !doStop then doStop := false else let
           val descs = (!descs)
           fun doPoll timeout = OS.IO.poll (map #1 descs, timeout)
           fun noTimeout is =
               recur (is & descs) (fn lp =>
                  fn [] & _ => run ef
                   | _ & [] => fail "run"
                   | i::is & (da as (d, action))::das =>
                     if OS.IO.infoToPollDesc i = d then
                        (action i ; lp (is & da::das))
                     else
                        lp (i::is & das))
        in
           case List.pop timeouts of
              NONE => noTimeout (doPoll NONE)
            | SOME (absTime, action) =>
              case doPoll let
                      open Time
                   in SOME (Cmp.max compare (zeroTime, absTime - now ()))
                   end of
                 [] => (action () ; run ef)
               | is => (List.push timeouts (absTime, action) ; noTimeout is)
        end)
end
