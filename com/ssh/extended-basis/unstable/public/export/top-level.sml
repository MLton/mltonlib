(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(**
 * Structure defining bindings intended to be available at the top-level.
 * This structure exists for the benefit of Standard ML implementations
 * that do not support introducing new top-level bindings.
 *
 * See also: <infixes.sml>
 *)
structure TopLevel = struct
   (** == Basic == *)

   val eq = Basic.eq
   val notEq = Basic.notEq
   val fail = Basic.fail
   val fails = Basic.fails
   val failing = Basic.failing
   val raising = Basic.raising
   val recur = Basic.recur
   val repeat = Basic.repeat
   val undefined = Basic.undefined

   (** == Effect == *)

   val obs = Effect.obs

   (** == Exn == *)

   val after = Exn.after
   val finally = Exn.finally (** DEPRECATED: This is an Alice ML reserved word. *)
   val try = Exn.try

   (** == Fn == *)

   val const = Fn.const
   val curry = Fn.curry
   val eta = Fn.eta
   val flip = Fn.flip
   val id = Fn.id
   val seal = Fn.seal
   val uncurry = Fn.uncurry

   val op /> = Fn./>
   val op </ = Fn.</
   val op <\ = Fn.<\
   val op >| = Fn.>|
   val op \> = Fn.\>
   val op |< = Fn.|<

   (** == CPS == *)

   val pass = CPS.return

   (** == Fold == *)

   val $ = Fold.$

   (** == FRU == *)

   val U = FRU.U

   (** == Iso == *)

   val op <--> = Iso.<-->

   (** == Lazy == *)

   type 'a lazy = 'a Lazy.t
   val delay = Lazy.delay
   val eager = Lazy.eager
   val force = Lazy.force
   val lazy = Lazy.lazy
   val memo = Lazy.memo

   (** == List == *)

   val pop = List.pop
   val push = List.push

   (** == Option == *)

   val isNone = Option.isNone

   (** == Pair == *)

   val swap = Pair.swap

   (** == Phantom == *)

   type yes = Phantom.yes
   type no = Phantom.no

   type ('f, 't, 'r) B = ('f, 't, 'r) Phantom.Bool.t
   type ('f, 't) T = ('f, 't) Phantom.Bool.T
   type ('f, 't) F = ('f, 't) Phantom.Bool.F

   (** == Product == *)

   datatype product = datatype Product.product

   (** == Ref == *)

   val op :=: = Ref.:=:

   (** == Sum == *)

   datatype sum = datatype Sum.sum
   val mirror = Sum.swap

   (** == TextIO == *)

   val println = TextIO.println
   val printlns = TextIO.printlns
   val prints = TextIO.prints

   (** == UnPr == *)

   val neg = UnPr.neg

   val op andAlso = UnPr.andAlso
   val op orElse = UnPr.orElse

   (** == Void == *)

   type void = Void.t
   val void = Void.void
end
