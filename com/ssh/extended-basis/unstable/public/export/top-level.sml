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
   val pass = Fn.pass
   val seal = Fn.seal
   val uncurry = Fn.uncurry

   val op /> = Fn./>
   val op </ = Fn.</
   val op <\ = Fn.<\
   val op >| = Fn.>|
   val op \> = Fn.\>
   val op |< = Fn.|<

   (** == Fold == *)

   val $ = Fold.$

   (** == Lazy == *)

   type 'a lazy = 'a Lazy.t
   val delay = Lazy.delay
   val eager = Lazy.eager
   val force = Lazy.force
   val lazy = Lazy.lazy
   val memo = Lazy.memo

   (** == Option == *)

   val isNone = Option.isNone

   (** == Phantom == *)

   type yes = Phantom.yes
   type no = Phantom.no

   (** == Product == *)

   datatype product = datatype Product.product

   (** == Ref == *)

   val op :=: = Ref.:=:

   (** == Sum == *)

   datatype sum = datatype Sum.sum

   (** == TextIO == *)

   val println = TextIO.println
   val printlns = TextIO.printlns
   val prints = TextIO.prints

   (** == UnPr == *)

   val negate = UnPr.negate

   val op andAlso = UnPr.andAlso
   val op orElse = UnPr.orElse

   (** == Void == *)

   type void = Void.t
   val void = Void.void
end
