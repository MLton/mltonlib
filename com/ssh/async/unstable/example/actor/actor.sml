(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * The goal here is to implement something that is as close to Scala's
 * Actors as possible while remaining threadless (which means that it is
 * not possible to get the exact same semantics).  This is neither
 * optimized nor supposed to demonstrate good SML programming style!  In
 * particular, Scala's Any type is approximated using SML's {exn} type and
 * Scala's partial functions are approximated using SML functions that may
 * raise {Match}.
 *
 * Bibliography:
 * - Philipp Haller and Martin Odersky:
 *   [http://lampwww.epfl.ch/~odersky/papers/jmlc06.pdf
 *    Event-Based Programming without Inversion of Control]
 * - Philipp Haller and Martin Odersky:
 *   [http://lamp.epfl.ch/~phaller/doc/haller07actorsunify.pdf
 *    Actors that Unify Threads and Events]
 *)

structure Actor :> sig
   type t

   structure Msg : sig
      type t = Exn.t
   end

   val new : t Effect.t -> t
   val start : t Effect.t
   val += : (t * Msg.t) Effect.t
   val receive : Msg.t Effect.t -> 'a
   (* The type says that receive can not return. *)
end = struct
   structure Msg = Exn

   datatype t =
      T of {body : t Effect.t,
            handler : Msg.t Effect.t Effect.t,
            send : Msg.t Effect.t}

   exception Receive of Msg.t Effect.t

   open Async

   fun new body = let
      val msgs = ref [] (* XXX inefficient *)
      val wakeupCh = SkipCh.new ()
      fun handler f =
          recur (!msgs, []) (fn loop =>
             fn ([], _) => when (SkipCh.take wakeupCh) (fn () => handler f)
              | (m::ms, fms) =>
                try (fn () => f m,
                     fn () => msgs := List.revAppend (fms, ms),
                     fn Match => loop (ms, m::fms)
                      | Receive f => (msgs := List.revAppend (fms, ms)
                                    ; handler f)))
      fun send msg = (msgs := !msgs @ [msg] ; SkipCh.send wakeupCh ())
   in
      T {body = body, handler = handler, send = send}
   end

   fun receive f = raise Receive f

   fun start (this as T {body, handler, ...}) =
       body this handle Receive f => handler f

   fun (T {send, ...}) += msg = send msg
end
