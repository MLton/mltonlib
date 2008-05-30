(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature MK_PARSEC_DOM = sig
   structure Sequence : SEQUENCE
   structure State : T
end

signature PARSEC = sig
   include MK_PARSEC_DOM

   include ETAEXP'
   include MONADP where type 'a monad = 'a etaexp

   type 'a t = 'a etaexp

   val parse : 'a t -> Sequence.t * State.t
               -> (Sequence.Pos.t, 'a * (Sequence.t * State.t)) Sum.t

   val getState : State.t t
   val setState : State.t -> Unit.t t

   val fromScan : ((Sequence.Elem.t, Sequence.t) Reader.t
                   -> ('a, Sequence.t) Reader.t) -> 'a t
   val fromReader : ('a, Sequence.t) Reader.t -> 'a t

   val guess : 'a t UnOp.t

   val elem : Sequence.Elem.t t
   val drop : Sequence.Elem.t UnPr.t -> Unit.t t
   val sat : Sequence.Elem.t UnPr.t -> Sequence.Elem.t t
   val take : Sequence.Elem.t UnPr.t -> Sequence.Elem.t List.t t

   val peek : 'a t UnOp.t

   val many : 'a t -> 'a List.t t
   val many1 : 'a t -> 'a List.t t

   val option : 'a -> 'a t UnOp.t

   val between : 'a t -> 'b t -> 'c t UnOp.t

   val sepBy : 'a t -> 'b t -> 'a List.t t
   val sepBy1 : 'a t -> 'b t -> 'a List.t t

   val skip : 'a t -> Unit.t t
   val skipMany : 'a t -> Unit.t t
   val skipMany1 : 'a t -> Unit.t t
end
