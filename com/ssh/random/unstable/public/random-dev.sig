(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * A signature for accessing some (unspecified) source of randomness
 * (e.g. /dev/random and /dev/urandom).  Modules implementing this
 * signature should not be used as general purpose random number
 * generators, but should rather be used to seed other pseudo random
 * number generators.
 *)
signature RANDOM_DEV = sig
   val seed : Word.t Option.t Thunk.t
   (**
    * Returns a high-quality random word.  A call to seed may block until
    * enough random bits are available.
    *)

   val useed : Word.t Option.t Thunk.t
   (**
    * Returns a random word.  If there aren't enough high-quality random
    * bits available, a lower quality random word will be returned.
    *)
end
