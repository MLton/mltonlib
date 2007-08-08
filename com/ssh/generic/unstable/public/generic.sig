(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Base signature for a module of directly usable generics.
 *)
signature GENERIC = sig
   structure Open : OPEN_CASES

   include CLOSED_CASES
      where type  'a      Rep.t = ('a,     Unit.t) Open.Rep.t
      where type  'a      Rep.s = ('a,     Unit.t) Open.Rep.s
      where type ('a, 'k) Rep.p = ('a, 'k, Unit.t) Open.Rep.p
end
