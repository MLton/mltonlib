(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * This signature specifies some notable extensions to the {WINDOWS} signature.
 *)
signature WINDOWS_EX = sig
   include WINDOWS

   structure EventLog : sig
      structure Type : sig
         include FLAGS where type flags_word = Word16.t
         type t = flags
         val auditFailure : t
         val auditSuccess : t
         val error : t
         val information : t
         val warning : t
      end
   end

   structure Module : sig
      eqtype t
      val getFileName : t Option.t -> String.t
   end

   structure File : sig
      val getShortName : String.t UnOp.t
      val copy : {from : String.t, to : String.t, failIfExists : Bool.t} Effect.t
   end

   structure Wait : sig
      type t

      val hash : t -> Word.t

      val compare : t Cmp.t

      datatype 'a result
        = ABANDONED of 'a
        | OBJECT of 'a
        | TIMEOUT

      val any : (t * 'a) List.t -> Time.time Option.t -> 'a result
      val all : (t * 'a) List.t -> Time.time Option.t -> 'a result
   end

   structure Semaphore : sig
      type t
      val create : {init : Int32.t, max : Int32.t, name : String.t Option.t} -> t
      val close : t Effect.t
      val release : t * Int32.t -> Int32.t
      val toWait : t -> Wait.t
   end

   structure Mutex : sig
      type t
      val create : {name : String.t Option.t, own : Bool.t} -> t
      val close : t Effect.t
      val release : t Effect.t
      val toWait : t -> Wait.t
   end

   structure Timer : sig
      type t
      val create : {manual : Bool.t, name : String.t Option.t} -> t
      val close : t Effect.t
      val setAbs : {timer : t,
                    due : Time.time,
                    period : Time.time Option.t} Effect.t
      val setRel : {timer : t,
                    due : Time.time,
                    period : Time.time Option.t} Effect.t
      val cancel : t Effect.t
      val toWait : t -> Wait.t
   end

   structure FileChange : sig
      structure Filter : sig
         include FLAGS where type flags_word = Word32.t
         type t = flags
         val fileName : t
         val dirName : t
         val attributes : t
         val size : t
         val lastWrite : t
         val security : t
      end

      type t
      val first : String.t * Bool.t * Filter.flags -> t
      val next : t Effect.t
      val close : t Effect.t
      val toWait : t -> Wait.t
   end

   structure Window : sig
      type t

      val find : {class : String.t Option.t, window : String.t  Option.t} -> t

      structure SW : sig
         type t
         val forceminimize : t
         val hide : t
         val maximize : t
         val minimize : t
         val restore : t
         val show : t
         val showdefault : t
         val showmaximized : t
         val showminimized : t
         val showminnoactive : t
         val showna : t
         val shownoactivate : t
         val shownormal : t
      end

      val show : t * SW.t -> Bool.t
   end
end
