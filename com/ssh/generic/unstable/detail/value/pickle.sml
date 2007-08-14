(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(************************************************************************)

structure HashMap :> sig
   type ('a, 'b) t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, 'b) t
   val insert : ('a, 'b) t -> ('a * 'b) Effect.t
   val find : ('a, 'b) t -> 'a -> 'b Option.t
   val numItems : ('a, 'b) t -> Int.t
end = struct
   open HashTable
   type ('a, 'b) t = ('a, 'b) hash_table
   fun new {eq, hash} = mkTable (hash, eq) (100, Subscript)
end

(************************************************************************)

signature HASH_UNIV = sig
   type t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, t) Iso.t
   val eq : t BinPr.t
   val hash : t -> Word.t
end

structure HashUniv :> HASH_UNIV = struct
   datatype t = T of {value : Univ.t,
                      methods : {eq : Univ.t BinPr.t, hash : Univ.t -> Word.t}}
   fun new {eq, hash} = let
      val (to, from) = Univ.Emb.new ()
      val methods = {eq = fn (l, r) => case (from l, from r)
                                        of (SOME l, SOME r) => eq (l, r)
                                         | _                => false,
                     hash = hash o valOf o from}
   in
      (fn value => T {value = to value, methods = methods},
       fn T r => valOf (from (#value r)))
   end
   fun eq (T l, T r) = #eq (#methods l) (#value l, #value r)
   fun hash (T r) = #hash (#methods r) (#value r)
end

(************************************************************************)

functor MkIstream (State : T) :> sig
   type 'a t
   val Y : 'a t Tie.t
   val run : State.t -> 'a t -> (Char.t, 'b) Reader.t -> ('a, 'b) Reader.t
   val read : Char.t t
   structure State : T where type t = State.t
   val getState : State.t t
   val setState : State.t -> Unit.t t
   include MONAD where type 'a monad = 'a t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)
   open Reader
   datatype t = T of {st : Univ.t, rd : (Char.t, Univ.t) Reader.t, us : State.t}
   type 'a t = ('a, t) Reader.t
   val Y = Tie.function
   fun run us f cr = let
      val (to, from) = Univ.Iso.new ()
   in
      mapState (fn s => T {st = to s, rd = mapState (from, to) cr, us = us},
                fn T r => from (#st r))
               f
   end
   fun read (T {st, rd, us}) =
       Option.map (Pair.map (id, fn st => T {st=st, rd=rd, us=us})) (rd st)
   structure State = State
   fun getState (s as T {us, ...}) = SOME (us, s)
   fun setState us (T {st, rd, ...}) = SOME ((), T {st=st, rd=rd, us=us})
   structure Monad =
      MkMonad (type 'a monad = 'a t
               fun return a s = SOME (a, s)
               fun op >>= (rA, a2rB) s = case rA s
                                          of NONE        => NONE
                                           | SOME (a, s) => a2rB a s)
   open Monad
end

(************************************************************************)

functor MkOstream (State : T) :> sig
   type 'a t
   val Y : 'a t Tie.t
   val run : State.t -> ('a -> Unit.t t) -> (Char.t, 'b) Writer.t -> ('a, 'b) Writer.t
   val write : Char.t -> Unit.t t
   structure State : T where type t = State.t
   val getState : State.t t
   val setState : State.t -> Unit.t t
   include MONAD where type 'a monad = 'a t
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)
   open Writer
   datatype t = T of {st : Univ.t, wr : (Char.t, Univ.t) Writer.t, us : State.t}
   type 'a t = t -> 'a * t
   val Y = Tie.function
   fun run us f cw (a, s) = let
      val (to, from) = Univ.Iso.new ()
   in
      case f a (T {st = to s, wr = mapState (from, to) cw, us = us})
       of ((), T r) => from (#st r)
   end
   fun write c (T r) = ((), T {st = #wr r (c, #st r), wr = #wr r, us = #us r})
   structure State = State
   fun getState (s as T {us, ...}) = (us, s)
   fun setState us (T {st, wr, ...}) = ((), T {st=st, wr=wr, us=us})
   structure Monad =
      MkMonad (type 'a monad = 'a t
               fun return x s = (x, s)
               fun op >>= (mA, a2mB) s = uncurry a2mB (mA s))
   open Monad
end

(************************************************************************)

functor WordWithOps (Arg : WORD) = struct
   open Arg
   val ops = {wordSize = wordSize, orb = op orb, << = op <<, >> = op >>,
              isoWord8 = isoWord8}
end

(************************************************************************)

functor WithPickle (Arg : WITH_PICKLE_DOM) : PICKLE_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  8 * div >> << *` / mod ~>> /`
   infix  7 + - ^ andb +` -` ^`
   infix  6 xorb
   infixr 6 ::  @ ::` @`
   infix  5 > >= =  orb == =` < <= <>= ?=
   infix  4 <\ \>
   infixr 4 </ />
   infix  3 o <-->
   infix  2 andAlso >|
   infixr 2 |<
   infix  1 := orElse >>= >>& :=: += -= >>* >>@
   infixr 1 =<<
   infix  0 before <|> &` &
   infixr 0 -->
   (* SML/NJ workaround --> *)

   structure Word = WordWithOps (Word)
   structure Word32 = WordWithOps (Word32)
   structure Word64 = WordWithOps (Word64)
   structure LargeWord = WordWithOps (LargeWord)
   structure LargeRealWord = WordWithOps (CastLargeReal.Bits)
   structure RealWord = WordWithOps (CastReal.Bits)

   structure Dyn = HashUniv

   structure I = MkIstream (type t = (Int.t, Dyn.t) HashMap.t)
   structure O = MkOstream (type t = (Dyn.t, Int.t) HashMap.t)

   datatype 'a t = INT of {rd : 'a I.t, wr : 'a -> Unit.t O.t}
   type 'a s = Int.t -> {rd : Int.t -> 'a I.t, wr : 'a -> Unit.t O.t}

   structure Pickle = LayerRep
      (structure Outer = Arg.Rep
       structure Closed = struct
          type 'a t = 'a t and 'a s = 'a s and ('a, 'k) p = 'a t
       end)

   open Pickle.This

   fun pickle t =
       case getT t
        of INT r => O.run (HashMap.new {eq = Dyn.eq, hash = Dyn.hash}) (#wr r)
   fun unpickle t =
       case getT t
        of INT r => I.run (HashMap.new {eq = op =, hash = Word.fromInt}) (#rd r)

   fun fake msg = INT {rd = I.thunk (failing msg), wr = failing msg}

   val op <--> = Iso.<-->
   val swap = Iso.swap
   val word8Ichar = (Byte.byteToChar, Byte.charToByte)

   fun bits {wordSize=n, orb, <<, >>, isoWord8} (toBits, fromBits) = let
      val (toChar, fromChar) = word8Ichar <--> isoWord8
      fun alts ` op + =
          if      n <= 8  then `0
          else if n <= 16 then `0 + `8
          else if n <= 32 then `0 + `8 + `16 + `24
          else if n <= 64 then `0 + `8 + `16 + `24 + `32 + `40 + `48 + `56
          else fail "Too many bits"
      val rd = let
         open I
         fun ` n = read >>= (fn c => return (fromChar c << Word.fromInt n))
         fun l + r = map op orb (l >>* r)
      in
         map fromBits (alts ` op +)
      end

      fun wr v = let
         val bits = toBits v
      in
         alts (fn n => O.write (toChar (bits >> Word.fromInt n))) O.>>
      end
   in
       INT {rd = rd, wr = wr}
   end

   val int as INT {rd=rdInt, wr=wrInt} = bits Word.ops (swap Word.isoIntX)

   fun mutable {readProxy, readBody, writeWhole, self} = let
      val cyclic = Arg.mayBeCyclic self
      val tagD = #"\000" and tagR = #"\001"
      val (toDyn, fromDyn) = Dyn.new {eq = op =, hash = Arg.hash self}
      open I
      val rd =
          read >>& getState >>= (fn tag & mp =>
          if tag = tagD then
             readProxy >>= (fn proxy =>
             if cyclic
             then (HashMap.insert mp (HashMap.numItems mp, toDyn proxy)
                 ; readBody proxy >> return proxy)
             else (readBody proxy >>= (fn () =>
                   (HashMap.insert mp (HashMap.numItems mp, toDyn proxy)
                  ; return proxy))))
          else if tag = tagR then
             rdInt >>= (fn i =>
             case HashMap.find mp i
              of NONE   => fail "Corrupted pickle"
               | SOME d => return (fromDyn d))
          else fail "Corrupted pickle")
      fun wr v = let
         val d = toDyn v
         open O
      in
         getState >>= (fn mp =>
         case HashMap.find mp d
          of SOME i => write tagR >> wrInt i
           | NONE   => 
                if cyclic
                then (HashMap.insert mp (d, HashMap.numItems mp)
                    ; write tagD >> writeWhole v)
                else write tagD >> writeWhole v >>= (fn () =>
                     (HashMap.insert mp (d, HashMap.numItems mp)
                    ; return ())))
      end
   in
      INT {rd = rd, wr = wr}
   end

   fun seq {length, toSlice, getItem, fromList} (INT {rd=rdE, wr=wrE}) = let
      open O
      fun wr seq = let
         fun lp sl =
             case getItem sl
              of NONE         => return ()
               | SOME (e, sl) => wrE e >>= (fn () => lp sl)
      in
         wrInt (length seq) >>= (fn () => lp (toSlice seq))
      end
      open I
      val rd = rdInt >>= (fn n => let
                  fun lp (0, es) = return (fromList (rev es))
                    | lp (n, es) = rdE >>= (fn e => lp (n-1, e::es))
               in
                  if n < 0 then fail "Corrupted pickle" else lp (n, [])
               end)
   in
      INT {rd = rd, wr = wr}
   end

   fun iso' get bT (a2b, b2a) = let
      val INT {rd, wr} = get bT
   in
      INT {rd = I.map b2a rd, wr = wr o a2b}
   end

   structure Layered = LayerDepCases
     (structure Outer = Arg and Result = Pickle

      fun iso        ? = iso' getT ?
      fun isoProduct ? = iso' getP ?

      fun isoSum bS (a2b, b2a) i = let
         val {rd, wr} = getS bS i
      in
         {rd = I.map b2a o rd, wr = wr o a2b}
      end

      fun op *` (lT, rT) = let
         val INT {rd=rL, wr=wL} = getP lT
         val INT {rd=rR, wr=wR} = getP rT
      in
         INT {rd = let open I in rL >>& rR end,
              wr = let open O in fn l & r => wL l >> wR r end}
      end

      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (lT, rT) = let
         val lN = Arg.numAlts lT
         val lS = getS lT
         val rS = getS rT
      in
         fn i => let
               val j = i+lN
               val {rd=rL, wr=wL} = lS i
               val {rd=rR, wr=wR} = rS j
            in
               {rd = fn i => if i < j
                             then I.map INL (rL i)
                             else I.map INR (rR i),
                wr = Sum.sum (wL, wR)}
            end
      end
      val unit = INT {rd = I.return (), wr = fn () => O.return ()}
      fun C0 _ i = {rd = const (I.return ()),
                    wr = fn () => O.write (chr i)}
      fun C1 _ t = let
         val INT {rd, wr} = getT t
      in
         fn i => {rd = const rd,
                  wr = fn v => let open O in write (chr i) >> wr v end}
      end
      fun data s = let
         val n = Arg.numAlts s
         val {rd, wr} = getS s 0
         open I
      in
         INT {rd = map ord read >>= (fn i => if n <= i
                                             then fail "Corrupted pickle"
                                             else rd i),
              wr = wr}
      end

      fun Y ? = let open Tie in iso (I.Y *` function) end
                   (fn INT {rd=r, wr=w} => r&w, fn r&w => INT {rd=r, wr=w}) ?

      fun op --> _ = fake "Pickle.--> unsupported"

      fun refc t = let
         val INT {rd, wr} = getT t
      in
          mutable {readProxy = I.thunk (ref o const (Arg.some t)),
                   readBody = fn proxy => I.map (fn v => proxy := v) rd,
                   writeWhole = wr o !,
                   self = Arg.refc ignore t}
      end

      fun array t = let
         val INT {rd, wr} = getT t
         fun readBody a = let
            open I
            fun lp i = if i = Array.length a
                       then return ()
                       else rd >>= (fn e => (Array.update (a, i, e) ; lp (i+1)))
         in
            lp 0
         end
         fun writeWhole a = let
            open O
            fun lp i = if i = Array.length a
                       then return ()
                       else wr (Array.sub (a, i)) >>= (fn () => lp (i+1))
         in
            wrInt (Array.length a) >>= (fn () => lp 0)
         end
      in
          mutable {readProxy = I.map (Array.array /> Arg.some t) rdInt,
                   readBody = readBody,
                   writeWhole = writeWhole,
                   self = Arg.array ignore t}
      end

      fun list t = seq {length = List.length, toSlice = id,
                        getItem = List.getItem, fromList = id} (getT t)

      fun vector t = seq {length = Vector.length, toSlice = VectorSlice.full,
                          getItem = VectorSlice.getItem,
                          fromList = Vector.fromList} (getT t)

      val exn : Exn.t t = fake "Pickle.exn unimplemented"
      fun regExn _ _ = ()

      val char = INT {rd = I.read, wr = O.write}
      val bool = iso' id char (swap Char.isoInt <--> Bool.isoInt)
      val int = int
      val real = bits RealWord.ops CastReal.isoBits
      val string = seq {length = String.length, toSlice = Substring.full,
                        getItem = Substring.getc, fromList = String.fromList}
                       char
      val word = bits Word.ops Iso.id

      val largeInt = let
         fun to i = let
            val buffer = Buffer.new ()
            fun hexToInt c =
                ord c - (if      Char.inRange (#"0", #"9") c then ord #"0"
                         else if Char.inRange (#"a", #"f") c then ord #"a" - 10
                         else if Char.inRange (#"A", #"F") c then ord #"A" - 10
                         else fail "Bug in LargeInt.fmt")
            fun pack s =
                if Int.isOdd (Substring.size s) then pl (0, s) else lp s
            and lp s =
                case Substring.getc s
                 of NONE        => ()
                  | SOME (c, s) => pl (hexToInt c, s)
            and pl (i, s) =
                case Substring.getc s
                 of NONE        => fail "Bug"
                  | SOME (c, s) =>
                    (Buffer.push buffer (chr (hexToInt c * 16 + i)) ; lp s)
         in
            Buffer.push buffer (if i < 0 then #"\001" else #"\000")
          ; pack (Substring.full (LargeInt.fmt StringCvt.HEX (abs i)))
          ; Buffer.toString buffer
         end
         fun from s = let
            val buffer = Buffer.new ()
            fun intToHex i = chr (i + (if i<10 then ord #"0" else ord #"A"-10))
            fun lp s =
                case Substring.getc s
                 of NONE        => ()
                  | SOME (c, s) =>
                    (Buffer.push buffer (intToHex (Int.rem (ord c, 16)))
                   ; Buffer.push buffer (intToHex (Int.quot (ord c, 16)))
                   ; lp s)
         in
            if size s < 2 then fail "Corrupted pickle" else ()
          ; case String.sub (s, 0)
             of #"\000" => ()
              | #"\001" => Buffer.push buffer #"~"
              | _       => fail "Corrupted pickle"
          ; lp (Substring.triml 1 (Substring.full s))
          ; case LargeInt.scan StringCvt.HEX Substring.getc
                               (Substring.full (Buffer.toString buffer))
             of NONE        => fail "Corrupted pickle"
              | SOME (i, _) => i
         end
      in
         iso' id string (to, from)
      end
      val largeReal = bits LargeRealWord.ops CastLargeReal.isoBits
      val largeWord = bits LargeWord.ops Iso.id

      val word8  = iso' id char word8Ichar
      val word32 = bits Word32.ops Iso.id
      val word64 = bits Word64.ops Iso.id)

   open Layered
end
