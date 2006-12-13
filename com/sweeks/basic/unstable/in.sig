(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature IN = sig

   type t
   (**
    * The type of input streams.  See File.{openIn,withIn} for functions that
    * open files as instreams.
    *)

   val close: t -> Unit.t
   (**
    * close s closes input stream s.
    *)
   val lines: t -> String.t Seq.t
   (**
    * lines s returns a sequence of the lines in s.  The sequence is delayed.
    *)
   val get1: t -> Char.t Option.t
   (**
    * get1 s gets a single character from s.
    *)
   val getAll: t -> String.t
   (**
    * getAll s returns a String.t containing the rest of the characters in s.
    *)
   val getLine: t -> String.t Option.t
   (**
    * getLine s consumes a single line of s.
    *)
   val standard: t
   (**
    * The standard input stream (stdin).
    *)

end
