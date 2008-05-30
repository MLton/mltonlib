(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Generic

(* This is a simple example of using the {Uniplate} generic.  The program
 * reads a term given as an argument and shows the canonized version of
 * the term.  Canonization renames bindings so that alpha equivalent terms
 * have the same representation. *)

(* The {Lambda} module defines the representation of the terms of our toy
 * language. *)
structure Lambda = struct
   (* Identifier representation: *)
   structure Id : sig
      type t
      val t : t Rep.t
   end = struct
      open String
      val t = string
   end

   datatype t =
      FUN  of Id.t * t
    | APP  of t Sq.t
    | REF  of Id.t
    | INT  of Int.t
    | PLUS of t Sq.t
   (* ... *)

   (* Type representation for terms: *)
   val t = Tie.fix Y (fn t =>
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
               INL ? => FUN ?))
   (* Note that the term isomorphism is written in a pattern whose
    * complexity is linear in the number of variants. *)
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

(* {free term} returns a set of the free variables in the given {term}: *)
local
   open Set
   val refs = fn REF id      => singleton id | _ => empty
   val decs = fn FUN (id, _) => singleton id | _ => empty
in
   fun free term =
       difference
        (union (refs term,
                reduceC Lambda.t empty union free term),
         decs term)
end
(* To understand how the {free} function works, note that the {refs} and
 * {decs} functions return just the immediate variable references and
 * declarations in the given term.  They don't process the term
 * recursively.  To process the entire term recursively, the {free}
 * function uses {reduceC} to process the immediate subterms of the term
 * using itself, {free}, as the reducer.
 *
 * The {reduceC} function, obtained here via the type representation,
 * saves us from pattern matching over all the variants.  Only the
 * variable reference and declaration variants need to be treated
 * explicitly. *)

(* {renameFree it to term} renames free variables named {it} to {to} in
 * the given {term}: *)
fun renameFree it to term = let
   fun recurse () = transformC t (renameFree it to) term
in
   case term
    of FUN (v, _) => if v = it then term else recurse ()
     | REF v      => if v = it then REF to else term
     | _          => recurse ()
end
(* Except for using {transformC} rather than {reduceC}, the {renameFree}
 * function uses essentially the same pattern as the {free} function.  The
 * variable reference and declaration variants are first examined
 * explicitly.  Then, if necessary, {renameFree} uses {transformC} to
 * process the immediate subterms of the term using itself as the
 * transformer. *)

(* {countFuns term} returns the number of {FUN} variants in the given
 * {term}: *)
val countFuns =
    reduceU t 0 op + (fn FUN _ => 1 | _ => 0)

(* {canonize term} gives canonic names to all bound variables in the
 * given {term}: *)
val canonize =
    transformU t (fn FUN (v, t) => let
                        val n = countFuns t
                        val v' = Int.toString n
                     in
                        FUN (v', renameFree v v' t)
                     end
                   | other => other)
(* Here the canonic name of a bound variable is the number of {FUN}
 * subterms in the body of the {FUN} term that introduces the variable. *)

(* Here is an example term: *)
val exampleTerm =
    APP (FUN ("x", APP (REF "x", REF "x")),
         FUN ("x", FUN ("x", APP (REF "x", APP (REF "x", REF "x")))))

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
