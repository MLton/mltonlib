(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Extended {OS_FILE_SYS} signature.
 *)
signature OS_FILE_SYS = sig
   include BASIS_OS_FILE_SYS

   val foldDir : (String.t * 'a -> 'a) -> 'a -> String.t -> 'a
   (** Fold the list of filenames from the specified directory. *)

   val listDir : String.t -> String.t List.t
   (**
    * {listDir} is equivalent to {foldDir op :: []}.  Note that the order
    * in which filenames appear in the returned list is unspecified.
    *)
end
