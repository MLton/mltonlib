(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Generic

(**
 * This is a simple example of using the {Reduce} and {Transform}
 * generics.  The program reads a term given as an argument and shows the
 * canonized version of the term.  Canonization renames bindings so that
 * alpha equivalent terms have the same representation.
 *)

(* The {Lambda} module defines the representation of the terms of our toy
 * language.  Identifiers are just strings.  Crucial to the use of the
 * {Reduce} and {Transform} generics is that the terms of the language are
 * defined as a fixed point of a functor, {f}.  This allows the {Reduce}
 * and {Transform} generics to operate on all the subterms of a given
 * term.
 *
 * The commented ellipsis in the definition of the term functor suggests
 * that one could add further variants to the term.  Doing so means that
 * the functor type representation would also need to be changed.
 * However, unless new binding or variable reference forms are added,
 * other definitions would not need to be changed. *)
structure Lambda = struct
   (* Identifier representation: *)
   structure Id : sig
      type t
      val t : t Rep.t
   end = struct
      open String
      val t = string
   end

   (* Term functor: *)
   datatype 't f =
      FUN  of Id.t * 't
    | APP  of 't Sq.t
    | REF  of Id.t
    | INT  of Int.t
    | PLUS of 't Sq.t
   (* ... *)

   (* Type representation for the term functor: *)
   fun f t =
       iso (data (C1'"FUN" (tuple2 (Id.t, t))
               +` C1'"APP" (sq t)
               +` C1'"REF" Id.t
               +` C1'"INT" int
               +` C1'"PLUS" (sq t)))
           (fn PLUS ? => INR ? | ? => INL (case ? of
               INT ?  => INR ? | ? => INL (case ? of
               REF ?  => INR ? | ? => INL (case ? of
               APP ?  => INR ? |
               FUN ?  => INL ? | _ => fail "bug"))),
            fn INR ? => PLUS ? | INL ? => case ? of
               INR ? => INT ?  | INL ? => case ? of
               INR ? => REF ?  | INL ? => case ? of
               INR ? => APP ?  |
               INL ? => FUN ?)
   (* Note that the term isomorphism is written in a pattern whose
    * complexity is a linear in the number of variants in the term. *)

   (* A fixed point of the term functor: *)
   datatype t = IN of t f
   fun out (IN ?) = ?

   (* Type representation constructor for use with the {Reduce} and
    * {Transform} generics. *)
   fun t' t = iso (data (C1'"IN" (f t))) (out, IN)

   (* Type representation for the fixed point: *)
   val t = Tie.fix Y t'
end

open Lambda

(* The {Set} structure implements a naive set for our example: *)
structure Set = struct
   val t = list
   val empty = []
   val isEmpty = null
   fun singleton x = [x]
   fun union (xs, ys) = List.nubByEq op = (xs @ ys)
   fun difference (xs, ys) = List.filter (not o List.contains ys) xs
end

(* {free term} returns a set of the free variables in the given term. *)
local
   open Set
   val refs = fn REF id      => singleton id | _ => empty
   val decs = fn FUN (id, _) => singleton id | _ => empty
in
   fun free term =
       difference
          (union (refs (out term),
                  makeReduce empty union free Lambda.t Lambda.t' term),
           decs (out term))
end

(* {renameFree it to term} renames free variables named {it} to {to} in
 * the given {term}. *)
fun renameFree it to term = let
   fun recurse term =
       makeTransform (renameFree it to) t t' term
in
   case out term
    of FUN (v, _) => if v = it then term else recurse term
     | REF v      => if v = it then IN (REF to) else term
     | _          => recurse term
end

(* {countFuns term} returns the number of {FUN} variants in the given
 * {term}. *)
local
   val countHere = fn IN (FUN _) => 1 | _ => 0
in
   fun countFuns term =
       countHere term + makeReduce 0 op + countFuns t t' term
end

(* {canonize term} gives canonic names to all bound variables in the
 * given term.  Here the canonic name of a variable is the number of {FUN}
 * subterms contained within the body of the {FUN} term that introduces
 * the variable. *)
local
   fun canonizeHere term =
       case out term
        of FUN (v, t) => let
              val n = countFuns t
              val v' = Int.toString n
           in
              IN (FUN (v', renameFree v v' t))
           end
         | _ => term
in
   fun canonize term =
       canonizeHere (makeTransform canonize t t' term)
end

val exampleTerm =
    IN (APP (IN (FUN ("x",
                      IN (APP (IN (REF "x"), IN (REF "x"))))),
             IN (FUN ("x",
                      IN (FUN ("x",
                               IN (APP (IN (REF "x"),
                                        IN (APP (IN (REF "x"),
                                                 IN (REF "x")))))))))))

(* {say header term} prints out the {header} and the given {term} along
 * with a list of the free variables, if any, within the given {term}. *)
fun say header term = let
   open Prettier
   fun labelled label data = nest 3 (group (txt label <$> data))
   val noConNest = let open Fmt in default & conNest := NONE end
   val msg = labelled header (squotes (nest 1 (fmt Lambda.t noConNest term)))
   val freeVars = free term
   val msg = if Set.isEmpty freeVars
             then msg
             else msg <$>
                  labelled "where the free variables are:"
                           (pretty (Set.t Id.t) freeVars)
in
   println (SOME 74) msg
end

(* The main program just reads a given term and shows the canonized
 * version or shows an example term.
 *)
val () =
    case CommandLine.arguments ()
     of [e] => say "And here is the canonized term:" (canonize (read t e))
      | _   => say "Give me a term, for example:" exampleTerm

(* Instead of using the {Transform} generic, one could also use the more
 * flexible (and somewhat experimental) {Fmap} generic.  (This example was
 * initially implemented before the {Fmap} generic.)  Rewriting this
 * example using {Fmap} might be a fun exercise.
 *
 * Another unrelated exercise would be to design a concrete syntax for
 * the language and write a parser and pretty-printer for the concrete
 * syntax. *)
