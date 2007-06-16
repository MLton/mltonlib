(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for a generic dummy value.  In SML, dummy values are needed
 * for things such as computing fixpoints and building cyclic values.
 *
 * This generic value is unlikely to be directly useful in application
 * programs and is more likely to be used internally in the implementation
 * of some other generic values (e.g. pickling).
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
    * Removes the dummy value from the given representation.  This can be
    * used for encoding abstract types that can not be given dummy values.
    *)
end

signature DUMMY_GENERIC = sig
   include OPEN_GENERIC DUMMY
   sharing Rep = Dummy
end
