(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkClosedRep (type 'a t) : CLOSED_REP = struct
   type  'a      t = 'a t
   type  'a      s = 'a t
   type ('a, 'k) p = 'a t
end
