(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * Module level configurable debugging framework.
 *)

(* XXX This design and implementation is experimental and likely to change.
 *     Feedback is welcome!
 *)

signature DBG = sig
   exception Assertion
   val check : Bool.t -> Exn.t Effect.t
   val verify : Bool.t Effect.t
   val assert : Int.t -> Bool.t Thunk.t Effect.t
   val log : Int.t -> String.t Thunk.t Effect.t
end

structure DbgControl = struct
   type module =
        {name : String.t,
         assertLevel : Int.t Ref.t,
         logLevel : Int.t Ref.t,
         output : (String.t * String.t) Effect.t Ref.t}
end

signature DBG_CONTROL = sig
   type module = DbgControl.module
   val app : module Effect.t Effect.t
end

signature DBG_OPT = sig
   val name : String.t
   val enableLog : Bool.t
   val enableAssert : Bool.t
end

structure DbgDefs :> DBG_OPT = struct
   val name = ""
   val enableLog = true
   val enableAssert = true
end

structure DbgControl = struct
   open DbgControl

   exception Assertion

   fun check b e = if b then () else raise e
   fun verify b = check b Assertion
   fun output (name, msg) =
       TextIO.output (TextIO.stdErr, concat [name, ": ", msg, "\n"])

   local
      val modules = ref ([] : module list)
   in
      fun register m = modules := m :: !modules
      fun app ef = List.app ef (!modules)
   end
end

functor MkDbg (Opt : DBG_OPT) :> DBG = struct
   open DbgControl Opt

   val output = ref output

   val assertLevel = ref 0
   fun assert l t =
       if not enableAssert orelse !assertLevel < l then ()
       else verify (t ())

   val logLevel = ref 0
   fun log l m =
       if not enableLog orelse !logLevel < l then ()
       else !output (name, m ())

   val () = register
               {name = name,
                assertLevel = assertLevel,
                logLevel = logLevel,
                output = output}
end

structure DbgControl :> DBG_CONTROL = DbgControl
