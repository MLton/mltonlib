(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
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
end
