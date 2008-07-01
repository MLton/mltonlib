(* Copyright (C) 2007-2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * The {Cvt : CVT} module provides for relatively concise ad hoc text
 * formatting.  It is designed to yield a notation reminiscent of typical
 * "printf" facilities found in many languages.
 *
 * The intention is that a user opens the {Cvt} structure for use.  To
 * reduce the chance of shadowing user bindings, while allowing concise
 * notation, the values in the {Cvt} structure use upper-case names.
 *
 * For brevity, many of the identifiers use only a single letter.  In
 * addition, several combinators use first-class selectors for selecting
 * options.  An argument to such a combinator is a first-class selector
 * that selects one of the possible alternatives (continuations) for the
 * formatter.  For example, the first argument to the {R} combinator, for
 * formatting reals, is such a selector.  Suppose you wish to format a real
 * in scientific notation with 3 digits after the decimal point.  To do
 * that you could write {R#S 3 aReal}, which is equivalent to {Real.fmt
 * (StringCvt.SCI (SOME 3)) aReal}.
 *
 * The primed formatters {D'}, {G'}, {I'}, and {R'} use "-" as sign rather
 * than SML's "~".
 *)
signature CVT = sig
   type 'a t = 'a -> String.t
   (** Type of formatters or conversion functions from values to strings. *)

   type ('c, 's) sel = ('c -> 's) -> 's
   (** Type of selectors parameterized formatters. *)

   (** == Basic Formatters == *)

   val C :        Char.t t  (** Same as {str}. *)
   val B :        Bool.t t  (** Same as {Bool.toString}. *)
   val D :         Int.t t  (** Same as {Int.toString} and {I#d}. *)
   val D':         Int.t t
   val X :        Word.t t  (** Same as {Word.toString} and {W#x}. *)
   val G :        Real.t t  (** Same as {Real.toString} and {R#g}. *)
   val G':        Real.t t

   val I : ({b :   Int.t t  (** {I#b = Int.fmt BIN} *)
           , o :   Int.t t  (** {I#o = Int.fmt OCT} *)
           , d :   Int.t t  (** {I#d = Int.fmt DEC} *)
           , x :   Int.t t  (** {I#x = Int.fmt HEX} *)
            }, 'k) sel

   val I': ({b : Int.t t
           , o : Int.t t
           , d : Int.t t
           , x : Int.t t
            }, 'k) sel

   val W : ({b :  Word.t t (** {W#b = Word.fmt BIN} *)
           , o :  Word.t t (** {W#o = Word.fmt OCT} *)
           , d :  Word.t t (** {W#d = Word.fmt DEC} *)
           , x :  Word.t t (** {W#x = Word.fmt HEX} *)
            }, 'k) sel

   val R : ({s :          Real.t t (** {R#s   = Real.fmt (SCI NONE)} *)
           , S : Int.t -> Real.t t (** {R#S n = Real.fmt (SCI (SOME n))} *)
           , f :          Real.t t (** {R#f   = Real.fmt (FIX NONE)} *)
           , F : Int.t -> Real.t t (** {R#F n = Real.fmt (FIX (SOME n))} *)
           , g :          Real.t t (** {R#g   = Real.fmt (GEN NONE)} *)
           , G : Int.t -> Real.t t (** {R#G n = Real.fmt (GEN (SOME n))} *)
           , e :          Real.t t (** {R#e   = Real.fmt EXACT} *)
            }, 'k) sel

   val R': ({s :          Real.t t
           , S : Int.t -> Real.t t
           , f :          Real.t t
           , F : Int.t -> Real.t t
           , g :          Real.t t
           , G : Int.t -> Real.t t
           , e :          Real.t t
            }, 'k) sel

   (** == Formatter Combinators == *)

   val A : 'a t -> 'a   Array.t t  (** Makes a formatter for arrays. *)
   val L : 'a t -> 'a    List.t t  (** Makes a formatter for lists. *)
   val O : 'a t -> 'a  Option.t t  (** Makes a formatter for options. *)
   val V : 'a t -> 'a  Vector.t t  (** Makes a formatter for vectors. *)

   (** == Formatting Utilities == *)

   val P :
     ({l :           Int.t -> String.t UnOp.t (** {P#l   n = padLeft  #" " n} *)
     , r :           Int.t -> String.t UnOp.t (** {P#r   n = padRight #" " n} *)
     , L : Char.t -> Int.t -> String.t UnOp.t (** {P#L c n = padLeft  c    n} *)
     , R : Char.t -> Int.t -> String.t UnOp.t (** {P#R c n = padRight c    n} *)
      }, 'k) sel
end
