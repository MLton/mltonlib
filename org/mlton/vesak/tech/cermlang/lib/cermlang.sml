(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure CerMLang :> CERMLANG = struct
   structure Msg = Exn

   structure Proc = struct
      datatype t = IN of {tid : CML.thread_id,
                          msgs : {mbox : Msg.t Mailbox.mbox,
                                  more : Msg.t List.t Ref.t}}
      local
         val {getMsgs, setMsgs} =
             case CML.newThreadProp (fn () => fail "thread prop")
              of {getFn, setFn, ...} => {getMsgs = getFn, setMsgs = setFn}
      in
         fun current () = IN {tid = CML.getTid (), msgs = getMsgs ()}
         fun new () = setMsgs {mbox = Mailbox.mailbox (), more = ref []}
         fun msgsOf (IN r) = #msgs r
         val msgs = getMsgs
      end
   end

   exception Time = Time.Time

   fun start ef = ignore (RunCML.doit (ef o Proc.new, NONE))

   fun spawn ef = let
      val i = SyncVar.iVar ()
   in
      (ignore o CML.spawn)
         (fn () => (Proc.new ()
                  ; SyncVar.iPut (i, Proc.current ())
                  ; ef ()))
    ; SyncVar.iGet i
   end
   val self = Proc.current
   local
      exception Timer of Unit.t Ref.t Option.t
      fun receive ident handler = let
         val {mbox, more} = Proc.msgs ()
         fun lpRecv tried =
             case Mailbox.recv mbox
              of Timer i => if i = ident
                            then (more := rev tried ; handler Time ())
                            else lpRecv tried
               | m       => try (fn () => handler m,
                                 fn th => (more := rev tried ; th ()),
                                 fn Match => lpRecv (m::tried)
                                  | other => (more := rev tried ; raise other))
         fun lpMsgs tried =
          fn []    => lpRecv tried
           | m::ms => try (fn () => handler m,
                           fn th => (more := ms @ tried ; th ()),
                           fn Match => lpMsgs (m::tried) ms
                            | other => (more := ms @ tried ; raise other))
      in
         lpMsgs [] (!more before more := [])
      end
   in
      fun recv handler = receive NONE handler
      fun recvIn time handler =
          case time
           of NONE => receive NONE handler
            | SOME period => let
                 val ident = SOME (ref ())
                 val mbox = #mbox (Proc.msgs ())
              in
                 (ignore o CML.spawn)
                    (fn () => (CML.sync (CML.timeOutEvt period)
                             ; Mailbox.send (mbox, Timer ident)))
               ; receive ident handler
              end
   end
   fun t <- m = Mailbox.send (#mbox (Proc.msgsOf t), m)
end
