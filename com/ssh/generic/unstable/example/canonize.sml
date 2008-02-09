(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Generic

(* This is a simple example of using the {Reduce} and {Transform}
 * generics.  The program reads a term given as an argument and shows the
 * canonized version of the term.  Canonization renames bindings so that
 * alpha equivalent terms have the same representation. *)

(* The {Lambda} module defines the representation of the terms of our toy
 * language.  Identifiers are just strings.  Crucial to the use of the
 * {Reduce} and {Transform} generics is that the terms of the language are
 * defined as a fixed point of a functor, {f}.  This allows the {Reduce}
 * and {Transform} generics to operate on all the immediate subterms of a
 * given term.
 *
 * The commented ellipsis in the definition of the term functor suggests
 * that one could add further variants to the term.  Doing so means that
 * the functor type representation would also need to be changed.
 * However, unless new binding or variable reference variants are added,
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
    * complexity is linear in the number of variants. *)

   (* A fixed point of the term functor: *)
   datatype t = IN of t f

   (* Type representation for the fixed point: *)
   val t = Tie.fix Y (fn t => iso (data (C1'"IN" (f t))) (fn IN ? => ?, IN))
end

open Lambda

(* Shorthands for reducing and transforming terms: *)
fun reduce ? = makeReduce f t ?
fun transform ? = makeTransform f t ?

(* The {Set} structure implements a naive set for our example: *)
structure Set = struct
   val t = list
   val empty = []
   val isEmpty = null
   fun singleton x = [x]
   fun union (xs, ys) = List.nubByEq op = (xs @ ys)
   fun difference (xs, ys) = List.filter (not o List.contains ys) xs
end

(* {free term} returns a set of the free variables in the given {term}: *)
local
   open Set
   val refs = fn REF id      => singleton id | _ => empty
   val decs = fn FUN (id, _) => singleton id | _ => empty
in
   fun free (IN term) =
       difference
          (union (refs term,
                  reduce empty union free term),
           decs term)
end
(* To understand how the {free} function works, note that the {refs} and
 * {decs} functions return just the immediate variable references and
 * declarations in the given term.  They don't process the term
 * recursively.  To process the entire term recursively, the {free}
 * function uses {reduce} to process the immediate subterms of the term
 * using itself, {free}, as the reducer.
 *
 * The {reduce} function, obtained here via the type representation, saves
 * us from pattern matching over all the variants.  Only the variable
 * reference and declaration variants need to be treated explicitly. *)

(* {renameFree it to term} renames free variables named {it} to {to} in
 * the given {term}: *)
fun renameFree it to (IN term) = let
   fun recurse () = transform (renameFree it to) term
in
   IN (case term
        of FUN (v, _) => if v = it then term else recurse ()
         | REF v      => if v = it then REF to else term
         | _          => recurse ())
end
(* Except for using {transform} rather than {reduce}, the {renameFree}
 * function uses essentially the same pattern as the {free} function.  The
 * variable reference and declaration variants are first examined
 * explicitly.  Then, if necessary, {renameFree} uses {transform} to
 * process the immediate subterms of the term using itself as the
 * transformer. *)

(* {countFuns term} returns the number of {FUN} variants in the given
 * {term}: *)
local
   val countHere = fn FUN _ => 1 | _ => 0
in
   fun countFuns (IN term) =
       countHere term + reduce 0 op + countFuns term
end

(* {canonize term} gives canonic names to all bound variables in the
 * given {term}: *)
local
   val canonizeHere =
    fn FUN (v, t) => let
          val n = countFuns t
          val v' = Int.toString n
       in
          FUN (v', renameFree v v' t)
       end
     | other => other
in
   fun canonize (IN term) =
       IN (canonizeHere (transform canonize term))
end
(* Here the canonic name of a bound variable is the number of {FUN}
 * subterms in the body of the {FUN} term that introduces the variable. *)

(* Here is an example term: *)
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
   val msg = labelled header (squotes (nest 1 (fmt t noConNest term)))
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
 * version or shows an example term: *)
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
 * syntax.  This example just lazily uses the generic {read} and {show}
 * functions obtained via the type representation. *)
