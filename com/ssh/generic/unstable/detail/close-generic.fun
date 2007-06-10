(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor CloseGeneric (Arg : OPEN_GENERIC) :>
   CLOSED_GENERIC
      where type 'a Rep.t = ('a, Unit.t) Arg.Rep.t
      where type 'a Rep.s = ('a, Unit.t) Arg.Rep.s
      where type ('a, 'k) Rep.p = ('a, 'k, Unit.t) Arg.Rep.p =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   structure Rep : CLOSED_GENERIC_REP = struct
      type 'a t = ('a, Unit.t) Arg.Rep.t
      type 'a s = ('a, Unit.t) Arg.Rep.s
      type ('a, 'k) p = ('a, 'k, Unit.t) Arg.Rep.p
   end

   fun morph m = m (const ignore)

   fun iso ? = morph Arg.iso ?
   fun isoProduct ? = morph Arg.isoProduct ?
   fun isoSum ? = morph Arg.isoSum ?
   fun op *` ? = Arg.*` ignore ?
   fun T ? = Arg.T ignore ?
   fun R ? = Arg.R (const ignore) ?
   fun tuple ? = Arg.tuple ignore ?
   fun record ? = Arg.record ignore ?
   fun op +` ? = Arg.+` ignore ?
   fun C0 ? = Arg.C0 (const ()) ?
   fun C1 ? = Arg.C1 (const ignore) ?
   fun data ? = Arg.data ignore ?
   val unit = Arg.unit ()
   fun Y ? = Arg.Y Tie.unit ?
   fun op --> ? = Arg.--> ignore ?
   val exn = Arg.exn ()
   fun regExn ? = Arg.regExn (const ignore) ?
   fun array ? = Arg.array ignore ?
   fun refc ? = Arg.refc ignore ?
   fun vector ? = Arg.vector ignore ?
   val largeInt = Arg.largeInt ()
   val largeReal = Arg.largeReal ()
   val largeWord = Arg.largeWord ()
   val word8 = Arg.word8 ()
(* val word16 = Arg.word16 () (* Word16 not provided by SML/NJ *) *)
   val word32 = Arg.word32 ()
   val word64 = Arg.word64 ()
   fun list ? = Arg.list ignore ?
   val bool = Arg.bool ()
   val char = Arg.char ()
   val int = Arg.int ()
   val real = Arg.real ()
   val string = Arg.string ()
   val word = Arg.word ()
end
