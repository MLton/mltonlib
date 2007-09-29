(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A base signature for the "cases" of generics.
 *)
signature CASES = sig
   structure Open : OPEN_CASES
end
