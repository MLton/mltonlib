(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {UseLib} module that provides a simple {use} based
 * library for defining separately loadable program components.
 *
 * A program component is defined by creating a SML source file with the
 * "use" suffix and calling the {lib} function exactly once in it.  Such a
 * component can then be loaded either explicitly from the top-level by
 * calling {use}, or implicitly by loading some other component that
 * refers to it through {lib}.  Files specified to {lib} are loaded in the
 * specified order, but each component (file with suffix "use") is loaded
 * at most once.
 *
 * Paths given to the {lib} and {use} functions may contain references to
 * environment variables, e.g. "${VARIABLE}".  The predefined variable
 * "${SML_COMPILER}" gives the mnemonic name of the SML compiler.
 *
 * For example, suppose you have the file "foo.sig" containing the code:
 *
 *> signature FOO = sig val bar : unit -> unit end
 *
 * and the file "foo.sml" containing the code:
 *
 *> structure Foo : FOO = struct fun bar () = print "Foo.bar ()\n" end
 *
 * You could then define the component "foo.use" as:
 *
 *> lib ["foo.sig", "foo.sml"] ;
 *
 * You can then load it from the top-level by calling:
 *
 *> use "foo.use" ;
 *
 * or refer to it from other components:
 *
 *> lib [..., "path-to/foo.use", ...] ;
 *)
signature USE_LIB = sig
   val lib : string list -> unit
   (**
    * Defines a single program component composed of the specified
    * components (files with the suffix "use") and source files (files
    * with suffix other than "use").  Each call to {lib} must be in a
    * unique "use" file.  Relative paths given to {lib} are relative to
    * the directory of the "use" file containing the {lib} call.
    *)

   val use : string -> unit
   (**
    * Used from the top-level to load a specified program component or
    * source file.  Program components, with the "use" suffix, are loaded
    * at most once.
    *)
end
