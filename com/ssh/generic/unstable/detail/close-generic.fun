(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor CloseRep (include OPEN_REP) :>
   CLOSED_REP
      where type  'a      t = ('a,     Unit.t) t
      where type  'a      s = ('a,     Unit.t) s
      where type ('a, 'k) p = ('a, 'k, Unit.t) p =
struct
   type  'a      t = ('a,     Unit.t) t
   type  'a      s = ('a,     Unit.t) s
   type ('a, 'k) p = ('a, 'k, Unit.t) p
end

functor CloseCases (Arg : CASES) :>
   GENERIC
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Arg.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Arg.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Arg.Open.Rep.p =
struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   open Arg
   structure Rep = CloseRep (Open.Rep)

   fun morph m = m (const ignore)

   fun iso ? = morph Open.iso ?
   fun isoProduct ? = morph Open.isoProduct ?
   fun isoSum ? = morph Open.isoSum ?
   fun op *` ? = Open.*` ignore ?
   fun T ? = Open.T ignore ?
   fun R ? = Open.R (const ignore) ?
   fun tuple ? = Open.tuple ignore ?
   fun record ? = Open.record ignore ?
   fun op +` ? = Open.+` ignore ?
   fun C0 ? = Open.C0 (const ()) ?
   fun C1 ? = Open.C1 (const ignore) ?
   fun data ? = Open.data ignore ?
   val unit = Open.unit ()
   fun Y ? = Open.Y (Tie.id ()) ?
   fun op --> ? = Open.--> ignore ?
   val exn = Open.exn ()
   fun regExn0 ? = Open.regExn0 (const ignore) ?
   fun regExn1 ? = Open.regExn1 (const (const ignore)) ?
   fun array ? = Open.array ignore ?
   fun refc ? = Open.refc ignore ?
   fun vector ? = Open.vector ignore ?
   val fixedInt = Open.fixedInt ()
   val largeInt = Open.largeInt ()
   val largeReal = Open.largeReal ()
   val largeWord = Open.largeWord ()
   val word8 = Open.word8 ()
   val word32 = Open.word32 ()
(*
   val word64 = Open.word64 ()
*)
   fun list ? = Open.list ignore ?
   val bool = Open.bool ()
   val char = Open.char ()
   val int = Open.int ()
   val real = Open.real ()
   val string = Open.string ()
   val word = Open.word ()
end
