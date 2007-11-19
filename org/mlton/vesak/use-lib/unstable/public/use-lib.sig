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
   val lib : string list -> unit
   (**
    * Defines a library composed of the specified libraries and source
    * files.
    *)

   val use : string -> unit
   (**
    * Loads the specified library or uses the specified source file.
    * Environment variable references are allowed within the path.
    *)
end
