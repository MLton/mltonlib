(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This is an unstable experimental FFI utility library.
 *)

structure FFI = struct
   type 'a export = 'a Effect.t
   type 'a symbol = 'a Thunk.t * 'a Effect.t

   fun get ((th, _) : 'a symbol) = th ()
   fun set ((_, ef) : 'a symbol) x = ef x
end
