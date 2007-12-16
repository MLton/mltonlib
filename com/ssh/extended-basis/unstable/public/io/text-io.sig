(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** Extended {TEXT_IO} signature. *)
signature TEXT_IO = sig
   include BASIS_TEXT_IO

   val println : String.t Effect.t
   (** {println s} is equivalent to {prints [s, "\n"]}. *)

   val printlns : String.t List.t Effect.t
   (** {printlns} is equivalent to {println o concat}. *)

   val prints : String.t List.t Effect.t
   (** {prints} is equivalent to {print o concat}. *)

   val readFile : String.t -> vector
   (** Reads all data from the specified file. *)

   val writeFile : {file : String.t, data : vector} Effect.t
   (** Overwrites the specified file with the specified data. *)
end
