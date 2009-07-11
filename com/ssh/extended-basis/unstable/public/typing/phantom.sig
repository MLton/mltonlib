(* Copyright (C) 2007-2009 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Signature for the {Phantom} structure that defines some general purpose
 * Phantom types (types that have no values).
 *)
signature PHANTOM = sig
   type yes
   type no

   (** Substructure for phantom booleans and type-level conditionals. *)
   structure Bool : sig
      type ('f, 't, 'r) t
      (**
       * Type constructor for phantom booleans.
       *)

      type ('f, 't) T = ('f, 't, 't) t (** {true} *)
      type ('f, 't) F = ('f, 't, 'f) t (** {false} *)

      (** == Term Level Operations ==
       *
       * These are not normally used in actual programs.  However, these
       * can be used to exploit the SML type checker to compute type
       * expressions involving phantom booleans.
       *)

      val t : ('f, 't) T
      val f : ('f, 't) F

      val split : (('f1, 't1) F * ('f2, 't2) F,
                   ('f3, 't3) T * ('f4, 't4) T,
                   ('f5, 't5, 'r5) t * ('f6, 't6, 'r6) t) t ->
                  ('f5, 't5, 'r5) t * ('f6, 't6, 'r6) t

      val generalize :
          (('f1, 't1) F, ('f2, 't2) T, ('f, 't, 'r) t) t -> ('f, 't, 'r) t

      val iff : ('f, 't, 'r) t -> 't -> 'f -> 'r
      (** Does not return. *)

      val notb : (('f1, 't1) T, ('f2, 't2) F, ('f, 't, 'r) t) t
                 -> ('f, 't, 'r) t
      val andb : (('f1, 't1) F * ('f2, 't2) F, ('f3, 't3) F * ('f4, 't4) T,
                  'a * 'b) t * ('a, 'b, ('f, 't, 'r) t) t
                 -> ('f, 't, 'r) t
      val orb : (('f1, 't1) F * ('f2, 't2) T, ('f3, 't3) T * ('f4, 't4) T,
                 'a * 'b) t * ('a, 'b, ('f, 't, 'r) t) t
                -> ('f, 't, 'r) t
   end
end
