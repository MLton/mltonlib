(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkBounded (Core : BOUNDED_CORE) : BOUNDED = struct
   open Core
   type bounded_ex = bounded
   val (minValue, maxValue) = bounds
end
