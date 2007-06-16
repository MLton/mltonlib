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

   exception Dummy of Exn.t
   (**
    * This is raised when trying to extract the dummy value in case of
    * unfounded recursion or an abstract type that has not been given a
    * dummy value.
    *)

   val dummy : ('a, 'x) Dummy.t -> 'a
   (** Extracts the dummy value or raises {Dummy}. *)

   val withDummy : 'a Option.t -> ('a, 'x) Dummy.t UnOp.t
   (**
    * {withDummy NONE t} removes the dummy value from the given
    * representation {t} and {withDummy (SOME v) t} sets the dummy value
    * to {v} in the given representation {t}.
    *)
end

signature DUMMY_GENERIC = sig
   include OPEN_GENERIC DUMMY
   sharing Rep = Dummy
end
