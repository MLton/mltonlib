(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Pos : INTEGER = Int

structure Istream :> sig
   type t
   val run : ('a, t) Reader.t -> (Char.t, 'b) Reader.t -> ('a, 'b) Reader.t
   val read : (Char.t, t) Reader.t
   val pos : t -> Pos.t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   datatype t = T of {st : Univ.t, rd : (Char.t, Univ.t) Reader.t, ps : Pos.t}
   fun run f cr = let
      val (to, from) = Univ.Iso.new ()
   in
      Reader.mapState (fn s => T {st = to s,
                                  rd = Reader.mapState (from, to) cr,
                                  ps = 0},
                       fn T {st, ...} => from st)
                      f
   end
   fun read (T {st, rd, ps}) =
       Option.map
          (Pair.map (id, fn st => T {st = st, rd = rd, ps = ps + 1}))
          (rd st)
   fun pos (T r) = #ps r
end

structure Ostream :> sig
   type t
   val run : ('a, t) Writer.t -> (Char.t, 'b) Writer.t -> ('a, 'b) Writer.t
   val write : (Char.t, t) Writer.t
   val pos : t -> Pos.t
end = struct
   datatype t = T of {st : Univ.t, wr : (Char.t, Univ.t) Writer.t, ps : Pos.t}
   fun run f cw = let
      val (to, from) = Univ.Iso.new ()
   in
      Writer.mapState (fn s => T {st = to s,
                                  wr = Writer.mapState (from, to) cw,
                                  ps = 0},
                       fn T {st, ...} => from st)
                      f
   end
   fun write (c, T r) = T {st = #wr r (c, #st r), wr = #wr r, ps = #ps r + 1}
   fun pos (T r) = #ps r
end

functor WithPickle (Arg : WITH_PICKLE_DOM) : PICKLE_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +`
   infix  4 <\ \>
   infixr 4 </ />
   infix  2 >| andAlso
   infixr 2 |<
   infix  1 orElse >>=
   infix  0 &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   datatype 'a t =
      INT of {rd : ('a, {st : Istream.t}) Reader.t,
              wr : ('a, {st : Ostream.t}) Writer.t}
   type 'a s = Int.t -> 'a t

   structure Pickle = LayerRep
      (structure Outer = Arg.Rep
       structure Closed = struct
          type  'a      t = 'a t
          type  'a      s = 'a s
          type ('a, 'k) p = 'a t
       end)

   open Pickle.This

   fun pickle t =
       case getT t
        of INT {wr, ...} =>
           Ostream.run (Writer.mapState (fn s => {st = s}, #st) wr)
   fun unpickle t =
       case getT t
        of INT {rd, ...} =>
           Istream.run (Reader.mapState (fn s => {st = s}, #st) rd)

   val unsupported = INT {rd = fn _ => fail "Unsupported",
                          wr = fn _ => fail "Unsupported"}

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Pickle

      fun iso        _ _ = unsupported
      fun isoProduct _ _ = unsupported
      fun isoSum     _ _ _ = unsupported

      fun op *` _ = unsupported
      val T   = getT
      fun R _ = getT
      fun tuple  _ = unsupported
      fun record _ = unsupported

      fun op +` _ _ = unsupported
      val unit = unsupported
      fun C0 _ _ = unsupported
      fun C1 _ _ _ = unsupported
      fun data _ = unsupported

      fun Y ? = let open Tie in iso (function *` function) end
                   (fn INT {rd, wr} => rd & wr, fn r & w => INT {rd=r, wr=w}) ?

      fun op --> _ = unsupported

      fun refc _ = unsupported

      val int = unsupported

      fun list _ = unsupported

      fun array  _ = unsupported
      fun vector _ = unsupported

      val char = unsupported
      val string = unsupported

      val exn = unsupported
      fun regExn _ _ = ()

      val bool = unsupported
      val real = unsupported
      val word = unsupported

      val largeInt  = unsupported
      val largeReal = unsupported
      val largeWord = unsupported

      val word8  = unsupported
      val word32 = unsupported
      val word64 = unsupported)

   open Layered
end
