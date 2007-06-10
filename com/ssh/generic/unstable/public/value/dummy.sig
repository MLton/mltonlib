(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for a type-indexed family of dummy values.  In SML, dummy
 * values are needed for things such as computing fixpoints and building
 * cyclic values.
 *
 * This type-indexed function is unlikely to be directly useful in
 * application programs and is more likely to be used internally in the
 * implementation of some other type-indexed functions (e.g. pickling).
 *)
signature DUMMY = sig
   structure Dummy : OPEN_GENERIC_REP

   exception Dummy
   (**
    * This is raised when trying to extract the dummy value in case of
    * unfounded recursion or an abstract type that has not been given a
    * dummy value.
    *)

   val dummy : ('a, 'x) Dummy.t -> 'a
   (** Extracts the dummy value or raises {Dummy}. *)

   val noDummy : ('a, 'x) Dummy.t UnOp.t
   (**
    * Removes the dummy value from the given type-index.  This can be used
    * for encoding abstract types that can not be given dummy values.
    *)
end

signature DUMMY_GENERIC = sig
   include DUMMY OPEN_GENERIC
   sharing Dummy = Rep
end
