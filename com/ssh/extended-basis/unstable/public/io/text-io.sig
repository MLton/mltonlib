(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {TEXT_IO} signature. *)
signature TEXT_IO = sig
   include TEXT_IO

   val println : String.t Effect.t
   (**
    * Prints the given string to the standard output stream with a newline
    * and flushes the stream.
    *
    * This is available in the top-level environment as {println}.
    * {println s} is equivalent to:
    *
    *> (output (stdOut, s) ; output1 (stdOut, #"\n") ; flushOut stdOut)
    *)
end
