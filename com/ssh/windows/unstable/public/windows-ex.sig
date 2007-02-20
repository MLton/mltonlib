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
         include BIT_FLAGS
         val auditFailure : flags
         val auditSuccess : flags
         val error : flags
         val information : flags
         val warning : flags
      end
   end

   structure Module : sig
      eqtype t
      val getFileName : t Option.t -> String.t
   end

   structure Path : sig
      val getShortName : String.t UnOp.t
   end

   structure Wait : sig
      type t

      datatype 'a result
        = ABANDONED of 'a
        | OBJECT of 'a
        | TIMEOUT

      val any : (t * 'a) List.t -> Real.t -> 'a result
      val all : (t * 'a) List.t -> Real.t -> 'a result
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
      val toWait : t -> Wait.t
   end

   structure Timer : sig
      type t
      val create : {manual : Bool.t, name : String.t Option.t} -> t
      val close : t Effect.t
      val set : {timer : t, due : Int64.t, period : Int32.t} Effect.t
      val cancel : t Effect.t
      val toWait : t -> Wait.t
   end

   structure FileChange : sig
      structure Filter : sig
         include BIT_FLAGS
         val fileName : flags
         val dirName : flags
         val attributes : flags
         val size : flags
         val lastWrite : flags
         val security : flags
      end

      type t
      val first : String.t * Bool.t * Filter.flags -> t
      val next : t Effect.t
      val close : t Effect.t
      val toWait : t -> Wait.t
   end
end
