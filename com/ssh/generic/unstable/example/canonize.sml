(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

open Generic

(**
 * This is a simple example of using the Reduce and Transform generics.
 * The program reads a term given as an argument and prints the canonized
 * version of the term.  Canonization renames bindings so that alpha
 * equivalent terms have the same representation.
 *)

structure Lambda = struct
   structure Id = struct
      open String
      val t = string
   end

   datatype 't f =
      FUN  of Id.t * 't
    | APP  of 't Sq.t
    | REF  of Id.t
    | INT  of Int.t
    | PLUS of 't Sq.t
   (* ... *)

   datatype t = IN of t f
   fun out (IN ?) = ?

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

   fun t' t = iso (data (C1'"IN" (f t))) (out, IN)

   val t = Tie.fix Y t'
end

structure Set = struct
   val empty = []
   fun singleton x = [x]
   fun union (xs, ys) = List.nubByEq op = (xs @ ys)
   fun difference (xs, ys) = List.filter (not o List.contains ys) xs
end

local
   open Set Lambda
   val refs = fn REF id => singleton id | _ => empty
   val decs = fn FUN (id, _) => singleton id | _ => empty
in
   fun free term =
       difference
          (union (refs (out term),
                  makeReduce empty union free t t' term),
           decs (out term))
end

local
   open Set Lambda
in
   fun renameFree it to term = let
      fun recurse term =
          makeTransform (renameFree it to) t t' term
   in
      case out term
       of FUN (v, _) => if v = it then term else recurse term
        | REF v      => if v = it then IN (REF to) else term
        | _          => recurse term
   end
end

local
   open Set Lambda
   val countHere = fn IN (FUN _) => 1 | _ => 0
in
   fun countFuns term =
       countHere term + makeReduce 0 op + countFuns t t' term
end

local
   open Set Lambda
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

val exampleTerm = let
   open Lambda
in
   IN (APP (IN (FUN ("x",
                     IN (APP (IN (REF "x"), IN (REF "x"))))),
            IN (FUN ("x",
                     IN (FUN ("x",
                              IN (APP (IN (REF "x"),
                                       IN (APP (IN (REF "x"),
                                                IN (REF "x")))))))))))
end

val () = let
   open Prettier
   val noConNest = let open Fmt in default & conNest := NONE end
   fun say header term =
       println (SOME 74)
               (nest 3 (group (txt header <$>
                               squotes (nest 1 (fmt Lambda.t noConNest term)))))
in
   case CommandLine.arguments ()
    of [t] => say "And here is the canonized term:" (canonize (read Lambda.t t))
     | _   => say "Give me a term, for example:" exampleTerm
end
