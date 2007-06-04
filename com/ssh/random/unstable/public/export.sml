(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Exported Signatures == *)

signature RNG = RNG
signature RANDOM_GEN = RANDOM_GEN
signature RANDOM_DEV = RANDOM_DEV


(** == Exported Structures == *)

structure RandomDev : RANDOM_DEV = RandomDev
(** The default/system random device. *)

structure RanQD1Gen : RANDOM_GEN where type RNG.Seed.t = Word32.t = RanQD1Gen
(** A quick-and-dirty random value generator. *)


(** == Exported Functors == *)

functor MkRandomGen (RNG : RNG) : RANDOM_GEN = MkRandomGen (RNG)
(** Makes a random value generator combinators from a RNG. *)
