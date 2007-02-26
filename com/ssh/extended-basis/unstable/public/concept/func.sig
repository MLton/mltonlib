(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature FUNC = sig
   type 'a func
   val map : ('a -> 'b) -> 'a func -> 'b func
end

signature CFUNC = sig
   type 'a func
   val map : ('b -> 'a) -> 'a func -> 'b func
end

(************************************************************************)

signature FUNC' = sig
   type ('a, 'x1) func
   val map : ('a -> 'b) -> ('a, 'x1) func -> ('b, 'x1) func
end

signature CFUNC' = sig
   type ('a, 'x1) func
   val map : ('b -> 'a) -> ('a, 'x1) func -> ('b, 'x1) func
end
