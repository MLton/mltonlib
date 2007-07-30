(* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * This program note explores yet another approach to generic programming
 * in SML'97.  This is one is based on Lämmel and Jones' ``Scrap Your
 * Boilerplate'' (SYB) approach [1].  The SYB approach makes use of rank-2
 * types, a cast primitive, type classes, and boilerplate code generation.
 *
 * At the moment, only the {gmapT} functionality from the SYB paper is
 * implemented.
 *
 * [1] Scrap Your Boilerplate: A Practical Design Pattern for Generic
 *     Programming.
 *     Ralf Lämmel and Simon Peyton Jones.
 *     ACM SIGPLAN Notices, 38(3):26-37, March 2003.
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


val op <--> = Iso.<-->

val arrow = Fn.map
fun worra iso = arrow (Iso.swap iso)


structure G :> sig
   structure Void : sig
      type t
      structure Iso : sig
         type 'a t = ('a, t) Iso.t
      end
   end

   structure Type : sig
      type methods
      type 'a t

      val tyConA : 'a t Thunk.t
      val tyCon1 : 'a Void.Iso.t * (methods -> methods)
                   -> ('b Void.Iso.t -> ('c, 'a) Iso.t) -> 'b t -> 'c t
      val tyCon2 : 'a Void.Iso.t * (methods * methods -> methods)
                   -> ('b Void.Iso.t * 'c Void.Iso.t -> ('d, 'a) Iso.t)
                   -> 'b t * 'c t -> 'd t
      val pM : ('a -> {map : (methods -> Void.t UnOp.t) -> 'b -> 'b})
               -> 'b Void.Iso.t * ('a -> methods)
      val rM : ('a * methods * 'b Void.Iso.t
                -> {map:(methods -> Void.t UnOp.t) -> 'b -> 'b})
               -> 'b Void.Iso.t * ('a -> methods)
   end

   structure Map : sig
      val lift : 'a Type.t -> 'a UnOp.t -> Void.t Type.t -> Void.t UnOp.t
      val children : 'a Type.t -> (Void.t Type.t -> Void.t UnOp.t) -> 'a UnOp.t
      val bottomUp : (Void.t Type.t -> Void.t UnOp.t) -> 'a Type.t -> 'a UnOp.t
      val topDown : (Void.t Type.t -> Void.t UnOp.t) -> 'a Type.t -> 'a UnOp.t
   end
end = struct
   structure Void = Univ

   structure Type = struct
      datatype methods = M of {map : (methods -> Void.t UnOp.t) -> Void.t UnOp.t}
      datatype 'a t = T of 'a Void.Iso.t * methods

      fun tyConA () = T (Void.Iso.new (), M {map = const id})

      fun tyCon1 (tIu, t) iso (T (aIu, a)) =
          T (tIu <--> iso aIu, t a)
      fun tyCon2 (tIu, t) iso (T (aIu, a), T (bIu, b)) =
          T (tIu <--> iso (aIu, bIu), t (a, b))

      fun pM m = let
         val iso = Void.Iso.new ()
      in
         (iso, (fn {map} => M {map = worra iso o map}) o m)
      end

      fun rM m = let
         val iso = Void.Iso.new ()
      in
         (iso,
          fn a =>
             let open Tie in fix o iso function end
                (fn M {map} => map, fn map => M {map = map})
                (fn t => let
                       val {map} = m (a, t, iso)
                    in
                       M {map = worra iso o map}
                    end))
      end
   end

   open Type

   fun toVoid aM = T (Iso.id, aM)

   structure Map : sig
      val children : methods -> (methods -> Void.t UnOp.t) -> Void.t UnOp.t
      val bottomUp : (methods -> Void.t UnOp.t) -> methods -> Void.t UnOp.t
      val topDown : (methods -> Void.t UnOp.t) -> methods -> Void.t UnOp.t
   end = struct
      fun children (M r) f = #map r f
      fun bottomUp f aM = f aM o children aM (bottomUp f)
      fun topDown f aM = children aM (topDown f) o f aM
   end

   structure Map = struct
      fun lift (T ((a2u, u2a), _)) f _ u =
          try (fn () => u2a u,
               fn a => a2u (f a),
               fn Void.Univ => u | e => raise e)
      fun children (T (aIu, aM)) f =
          arrow aIu (Map.children aM (f o toVoid))
      fun bottomUp f (T (aIu, aM)) =
          arrow aIu (Map.bottomUp (f o toVoid) aM)
      fun topDown f (T (aIu, aM)) =
          arrow aIu (Map.topDown (f o toVoid) aM)
   end
end


structure G = struct
   open G

   structure Type = struct
      open Type

      val bool : Bool.t t = tyConA ()
      val char : Char.t t = tyConA ()
      val int  : Int.t  t = tyConA ()
      val real : Real.t t = tyConA ()
      val unit : Unit.t t = tyConA ()
      val word : Word.t t = tyConA ()

      val list = rM (fn (a, t, i) =>
                        {map = fn f => fn []   => []
                                        | x::r => f a x::arrow i (f t) r})
      val list = fn ? => tyCon1 list List.iso ?

      val pair = pM (fn (a, b) => {map = fn f => Pair.map (f a, f b)})
      val pair = fn ? => tyCon2 pair Pair.iso ?

      val option = pM (fn a => {map = fn f => Option.map (f a)})
      val option = fn ? => tyCon1 option Option.iso ?

      val vector = pM (fn a => {map = fn f => Vector.map (f a)})
      val vector = fn ? => tyCon1 vector Vector.iso ?
   end
end


local
   open G.Map G.Type
in
   val [SOME [~5]] =
       bottomUp (lift int ~) (list (option (list int))) [SOME [5]] ;

   val [NONE, SOME [2, 4, 3, 1]] =
       bottomUp (lift (list int) rev)
                (list (option (list int)))
                [NONE, SOME [1,2,3,4]] ;

   val (SOME ~1, [~2, ~3]) =
       bottomUp (lift int ~) (pair (option int, list int)) (SOME 1, [2, 3]) ;
end
