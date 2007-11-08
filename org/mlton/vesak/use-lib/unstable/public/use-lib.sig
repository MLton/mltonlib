(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {UseLib} module that provides a simple {use} based
 * library definition framework.
 *)
signature USE_LIB = sig
   val lib : {reqs : string list,
              self : string list} -> unit
   (**
    * Defines a library that depends on the {reqs} libraries and is
    * implemented by the {self} files.
    *)

   val use : string -> unit
   (**
    * Loads the specified library or uses the specified source file.
    * Environment variable references are allowed within the path.
    *)

   (**
    * Interface for recording flat traces of library loading.
    *)
   structure Trace : sig
      val load : string -> string list
      (** Load the specified library and return a list of used files. *)

      val fmt : {expandVars : bool} -> string list -> string
      (** Formats given trace as a flat use file. *)

      val disabled : (unit -> 'a) -> 'a
      (** Invoke thunk with trace disabled. *)
   end
end
