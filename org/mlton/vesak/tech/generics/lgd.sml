(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)


(*
 * This program note explores yet another approach to (datatype) generic,
 * or polytypic, values in SML'97.  This one uses "poor man's
 * existentials" and was inspired by Cheney & Hinze's ``A Lightweight
 * Implementation of Generics and Dynamics'' [1] and Stephen Weeks'
 * approach to type-indexed, or ad hoc polymorphic, functions in SML'97
 * [3].  The terminology is from [2].
 *
 * [1] A Lightweight Implementation of Generics and Dynamics.
 *     James Cheney and Ralf Hinze.
 *     Haskell '02: Proceedings of the 2002 ACM SIGPLAN workshop on Haskell.
 *     [http://citeseer.ist.psu.edu/540174.html]
 *
 * [2] Generics as a Library.
 *     Bruno C.d.S. Oliveira, Ralf Hinze and Andres Löh.
 *     In Henrik Nilsson, editor, Seventh Symposium on Trends in
 *     Functional Programming, 19-21 April, 2006, Nottingham, UK.
 *     [http://www.informatik.uni-bonn.de/~ralf/publications/GenLib.pdf]
 *
 * [3] A New Approach to Type-Indexed Values in SML.
 *     Stephen Weeks.
 *     Posted to the MLton-users mailing list, 28 Sep, 2006.
 *     [http://mlton.org/pipermail/mlton-user/2006-September/000914.html]
 *)


(*
   The following code uses the Extended Basis Library.  To try the code
   with SML/NJ, run the following prefix before evaluating the rest:

   val mltonLib = "../../../../.." ;
   val extBasisLib = mltonLib ^ "/com/ssh/extended-basis/unstable" ;
   CM.make (extBasisLib ^ "/basis.cm") ;
   use (extBasisLib ^ "/public/export/infixes.sml") ;
   open TopLevel ;
 *)


(* First a couple of shorthands. *)

val op <--> = Iso.<-->

type u = Univ.t
type 'a e = ('a, u) Iso.t


(* Signature for "structural cases". *)

signature CASES = sig
   type 'a t
   val iso : 'b t -> ('a, 'b) Iso.t -> 'a t
   val unit : Unit.t t
   val int : Int.t t
   val +` : 'a t * 'b t -> ('a, 'b) Sum.t t
   val *` : 'a t * 'b t -> ('a, 'b) Product.t t
   val Y : 'a t Tie.t
end

(*
 * The type variables in {CASES} are going to be mostly phantoms.  The
 * structural cases of a generic are actually used by the framework at
 * specific monomorphic types.  The upshot of defining the structural
 * cases as polymorphic functions in a structure is that they will be
 * type-checked (thanks to parametricity).
 *)


(* Type representation datatype. *)

structure Type = struct
   datatype t =
      UNIT
    | INT
    | SUM  of t Sq.t
    | PROD of t Sq.t
    | FIX  of t Ref.t * t
    | VAR  of t Ref.t

   fun Y ? =
       Tie.pure
          (fn () => let
                 val r = ref UNIT
              in
                 (VAR r, fn t => let val t = FIX (r, t) in r := t ; t end)
              end) ?
end

(*
 * Note the {FIX} and {VAR} constructors.  They are used to encode
 * recursive types.  The point here is to make it possible to implement a
 * fixed point witness, using the {Tie} module from the Extended Basis
 * Library, for the type constructor of an arbitrary generic (structural
 * cases).  IOW, we do not only compute a fixed point of the type
 * representation, but we also want to make it possible to compute a fixed
 * point of a given generic for the recursive type at hand.  In order to
 * do this, we are very explicit about recursion.
 *)


(*
 * Type representation cases.
 *
 * Somewhat tricky, but luckily this is a one time cost.
 *)

structure Type (* : CASES -- Sealed later! *) = struct
   open Type
   datatype 'a t = T of Type.t * 'a e

   val isoUnit : Unit.t e                   = Univ.newIso ()
   val isoInt  : Int.t e                    = Univ.newIso ()
   val isoSum  : (u, u) Sum.t Thunk.t e     = Univ.newIso ()
   val isoProd : (u, u) Product.t Thunk.t e = Univ.newIso ()

   val unit = T (UNIT, isoUnit)
   val int = T (INT, isoInt)
   fun iso (T (bU, bIu)) aIb = T (bU, bIu <--> aIb)
   fun op +` (T (lU, (l2u, u2l)), T (rU, (r2u, u2r))) = let
      val isoPoly = (fn INL l => (fn () => INL (l2u l))
                      | INR r => (fn () => INR (r2u r)),
                     Sum.map (u2l, u2r) o pass ())
   in
      T (SUM (lU, rU), isoSum <--> isoPoly)
   end
   fun op *` (T (lU, (l2u, u2l)), T (rU, (r2u, u2r))) = let
      val isoPoly = (fn l & r => (fn () => l2u l & r2u r),
                     Product.map (u2l, u2r) o pass ())
   in
      T (PROD (lU, rU), isoProd <--> isoPoly)
   end
   fun Y ? = let
      open Tie
   in
      iso (tuple2 (Type.Y, tuple2 (function, function)))
          (fn T ? => ?, T)
   end ?
end

(*
 * The universal type {u} and isomorphism {e} above implement the "poor
 * man's existentials" mentioned at the beginning.  See [1] for the
 * (trivial) Haskell version using existentials.
 *
 * Note the thunks in the sum and product cases.  The idea is to perform
 * coercions lazily.  For example, if you evaluate
 *
 *> equal (list int) ([], veryLongList)
 *
 * then only the first sum of the {veryLongList} should be coerced.
 *
 * Note also that the thunks do not appear in the {CASES}.  The framework
 * handles data coercions automatically behind the scenes.  This is
 * different from both Cheney & Hinze's and Weeks' approaches and leads to
 * comparatively concise definitions of generics (see {Eq} and {Ord} at
 * the end).
 *)


(* Signature for generics. *)

signature GENERIC = sig
   type 'a t
   val apply : 'a Type.t -> 'a t
end


(*
 * Functor to build a generic given the structural cases.
 *
 * What this does is to morph the structural cases to work 
 *)

functor Generic (C : CASES) : GENERIC = struct
   open Type C

   fun lookup r = Option.map Pair.snd o List.find (eq r o Pair.fst) o !
   fun insert (r, g) p = if isSome (lookup r p) then () else List.push p (r, g)

   val unit = iso unit (Iso.swap isoUnit)
   val int  = iso int  (Iso.swap isoInt)

   val isoSum  = Iso.swap (isoSum  <--> Thunk.iso)
   val isoProd = Iso.swap (isoProd <--> Thunk.iso)

   fun mk (p, t) =
       case t
        of UNIT        => unit
         | INT         => int
         | SUM  (l, r) => iso (mk (p, l) +` mk (p, r)) isoSum
         | PROD (l, r) => iso (mk (p, l) *` mk (p, r)) isoProd
         | VAR r       => mk (p, !r)
         | FIX (r, t)  => case lookup r p
                           of SOME g => g
                            | NONE   =>
                              Tie.fix Y (fn g => (insert (r, g) p ; mk (p, t)))

   fun apply (T (tU, e)) = iso (mk (ref [], tU)) e
end

(*
 * Note the treatment of recursive types.  To compute fixed points of the
 * generics (structural cases) we keep a (mutable) map of computed fixed
 * points.  Note that it is possible to get to a {VAR} without going
 * through a corresponding {FIX}, which is why the above calls {mk} in the
 * {VAR} case (the reference {r} points to the corresponding {FIX}).
 *)


(* Finally we seal the type representation. *)

structure Type : CASES = Type


(* Note that (all of) the above is part of the framework (one time cost). *)


(* Then we add a couple of type representation constructors. *)

structure Type = struct
   open Type

   fun option a =
       iso (unit +` a)
           (fn NONE => INL () | SOME a => INR a,
            fn INL () => NONE | INR a => SOME a)

   fun list a =
       (Tie.fix Y)
          (fn list_a =>
              iso (unit +` a *` list_a)
                  (fn [] => INL () | x::xs => INR (x & xs),
                   fn INL () => [] | INR (x & xs) => x::xs))

   datatype foo = FOO
                | FOO_BAR of bar
        and bar = BAR
                | BAR_FOO of foo

   val foo & bar =
       let open Tie in fix (Y *` Y) end
          (fn foo & bar =>
              iso (unit +` bar)
                  (fn FOO => INL () | FOO_BAR x => INR x,
                   fn INL () => FOO | INR x => FOO_BAR x) &
              iso (unit +` foo)
                  (fn BAR => INL () | BAR_FOO x => INR x,
                   fn INL () => BAR | INR x => BAR_FOO x))

   datatype useless = USELESS of (useless, useless) Product.t

   val useless =
       (Tie.fix Y)
          (fn useless =>
              iso (useless *` useless)
                  (fn USELESS ? => ?, USELESS))
end


(*
 * Below are implementations of generic equality and ordering relations.
 * The type constraints are for just for clarity.
 *)

structure Eq = Generic
  (open BinPr
   fun iso b (a2b, _) = map a2b b
   val unit = op = : Unit.t t
   val int = op = : Int.t t
   val op +` = Sum.equal
   val op *` = Product.equal
   val Y = Tie.function)

val equal : 'a Type.t -> 'a BinPr.t =
    Eq.apply

structure Ord = Generic
  (open Cmp
   fun iso b (a2b, _) = map a2b b
   val unit = Unit.compare
   val int = Int.compare
   val op +` = Sum.collate
   val op *` = Product.collate
   val Y = Tie.function)

val compare : 'a Type.t -> 'a Cmp.t =
    Ord.apply

(*
 * The above certainly works and the approach could be extended somewhat
 * further.  Try the {equal} and {compare} functions in a REPL!
 *
 * A problem with this approach is mutable types.  They can not be
 * supported through (the ideal) structural cases with specs of the
 * form:
 *
 *> val refc : 'a t -> 'a Ref.t t
 *> val array : 'a t -> 'a Array.t t
 *
 * Another problem with this approach is that the coercions to/from the
 * universal type are likely to have a significant cost.  This problem is
 * mostly due to lack of true existentials.
 *
 * Even with existentials, this approach would suffer from the
 * inflexibility that the user can not specify ad hoc cases [2] for
 * specific datatypes.  For example, picklers based on this approach need
 * to encode lists with a spine---unless the list type constructor is
 * added to the type representation.  IOW, this approach is not modular
 * according to the criteria in [2].
 *
 * One more deficiency with this particular design is the inability of
 * generics to call other generics (from the sum and product cases).  This
 * deficiency could probably be addressed.
 *)
