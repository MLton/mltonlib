(* Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(************************************************************************)

functor MkPassMonad (Arg : sig include MONAD_CORE T end) :> sig
   include MONAD_CORE
   val Y : 'a monad Tie.t
   val get : Arg.t monad
   val run : Arg.t -> 'a monad -> 'a Arg.monad
   val lift : 'a Arg.monad -> 'a monad
   val liftFn : ('a -> 'b Arg.monad) -> 'a -> 'b monad
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)
   datatype 'a monad = IN of Arg.t -> 'a Arg.monad
   fun return x = IN (const (Arg.return x))
   fun op >>= (IN aM, a2bM) =
       IN (fn t => Arg.>>= (aM t, (fn IN bM => bM t) o a2bM))
   fun Y ? = let open Tie in iso function end (fn IN ? => ?, IN) ?
   val get = IN Arg.return
   fun run t (IN aM) = aM t
   fun lift m = IN (const m)
   fun liftFn a2bM = lift o a2bM
end

(************************************************************************)

structure Istream :> sig
   include MONAD_CORE
   val run : 'a monad -> (Word8.t, 's) IOSMonad.t -> ('a, 's) IOSMonad.t
   val read : Word8.t monad
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)
   datatype t = T of {st : Univ.t, rd : (Word8.t, Univ.t) IOSMonad.t}
   type 'a monad = ('a, t) IOSMonad.t
   open IOSMonad
   fun run f cM =
       case Univ.Iso.new ()
        of (to, from) =>
           mapState (fn s => T {st = to s, rd = mapState (from, to) cM},
                     fn T r => from (#st r)) f
   fun read (T {st, rd}) = Pair.map (id, fn st => T {st=st, rd=rd}) (rd st)
end

(************************************************************************)

structure Ostream :> sig
   include MONAD_CORE
   val run : ('a -> Unit.t monad) -> (Word8.t -> (Unit.t, 's) IOSMonad.t)
                                  -> ('a      -> (Unit.t, 's) IOSMonad.t)
   val write : Word8.t -> Unit.t monad
end = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)
   datatype t = T of {st : Univ.t, wr : Word8.t -> (Unit.t, Univ.t) IOSMonad.t}
   type 'a monad = ('a, t) IOSMonad.t
   open IOSMonad
   fun run f c2uM =
       case Univ.Iso.new ()
        of (to, from) =>
           mapState (fn s => T {st = to s, wr = mapState (from, to) o c2uM},
                     fn T r => from (#st r)) o f
   fun write c (T r) =
       Pair.map (id, fn st => T {st = st, wr = #wr r}) (#wr r c (#st r))
end

(************************************************************************)

functor WithPickle (Arg : WITH_PICKLE_DOM) = let
   structure Result = struct
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
      infix  1 := orElse >>= >< :=: += -= >>* >>@
      infixr 1 =<<
      infix  0 before <|> &` &
      infixr 0 -->
      (* SML/NJ workaround --> *)

      structure Dyn = HashUniv

      structure I = let
         structure PMC = MkPassMonad
           (open Istream
            type t = Dyn.t ResizableArray.t)
         structure M = MkMonad (PMC)
      in
         struct
            open M
            structure Map = PMC
            structure Key = struct
               local
                  val dummy = #1 (Dyn.new {eq = undefined, hash = undefined}) ()
               in
                  val alloc = PMC.get >>= (fn arr =>
                              (ResizableArray.push arr dummy
                             ; return (ResizableArray.length arr)))
               end
            end
            fun run s = Istream.run o PMC.run s
            val read = PMC.lift Istream.read
            val Y = PMC.Y
         end
      end
      structure O = let
         structure PMC = MkPassMonad
           (open Ostream
            type t = Int.t Ref.t * (Dyn.t, Int.t) HashMap.t)
         structure M = MkMonad (PMC)
      in
         struct
            open M
            structure Map = struct
               val get = map #2 PMC.get
            end
            structure Key = struct
               val alloc = PMC.get >>= (fn (n, _) => (n := !n+1 ; return (!n)))
            end
            fun run s w = Ostream.run (PMC.run s o w)
            val write = PMC.liftFn Ostream.write
         end
      end

      datatype 'a t =
         P of {rd : 'a I.monad,
               wr : 'a -> Unit.t O.monad,
               sz : OptInt.t}
      fun rd (P r) = #rd r
      fun wr (P r) = #wr r
      fun sz (P r) = #sz r

      datatype 'a s =
         S of {rd : Int.t -> Int.t -> 'a I.monad,
               wr : Int.t -> 'a -> Int.t * Unit.t O.monad UnOp.t,
               sz : OptInt.t}

      fun fake msg = P {rd = I.thunk (failing msg), wr = failing msg, sz = NONE}

      val op <--> = Iso.<-->
      val swap = Iso.swap

      fun iso' (P {rd, wr, sz}) (a2b, b2a) =
          P {rd = I.map b2a rd, wr = wr o a2b, sz = sz}

      val unit = P {rd = I.return (), wr = O.return, sz = SOME 0}
      val word8 = P {rd = I.read, wr = O.write, sz = SOME 1}
      val char = iso' word8 (Byte.charToByte, Byte.byteToChar)
      val intAs8 = iso' word8 (swap Word8.isoInt)
      val intAs0 = iso' unit (ignore, const 0)

      (* Pickles a positive int using a variable length encoding. *)
      val size =
          P {rd = let
                open I
                fun lp (v, m) =
                    rd word8 >>= (fn b =>
                    if b < 0wx80
                    then return (v + Word8.toInt b * m)
                    else lp (v + Word8.toInt (Word8.andb (b, 0wx7F)) * m,
                             m * 0x80))
             in
                lp (0, 1)
             end,
             wr = let
                open O
                fun lp i =
                    if i < 0x80
                    then wr word8 (Word8.fromInt i)
                    else wr word8 (Word8.orb (0wx80, Word8.fromInt i)) >>=
                         (fn () => lp (Int.quot (i, 0x80)))
             in
                fn i => if i < 0 then fail "Negative size" else lp i
             end,
             sz = SOME 2}


      fun sized n aT =
          P {rd = let
                open I
             in
                rd size >>= (fn m =>
                if m <> n
                then fail "Wrong number of bits in pickle"
                else rd aT)
             end,
             wr = fn v => let open O in wr size n >> wr aT v end,
             sz = OptInt.+ (sz aT, SOME 1)}

      (* Encodes either 8, 16, 32, or 64 bits of raw data. *)
      fun bits (Ops.W {wordSize = n, orb, <<, ~>>, isoWord8 = (toW8, fromW8),
                       ...})
               (toBits, fromBits) = let
         fun alts ` op o =
             if      n <= 8  then `0w0
             else if n <= 16 then `0w0o`0w8
             else if n <= 32 then `0w0o`0w8o`0w16o`0w24
             else if n <= 64 then `0w0o`0w8o`0w16o`0w24o`0w32o`0w40o`0w48o`0w56
             else fail "Too many bits"
      in
         P {rd = let
               open I
               fun ` n = map (fn b => fromW8 b << n) (rd word8)
               fun l o r = map op orb (l >>* r)
            in
               map fromBits (alts ` op o)
            end,
            wr = fn v => let
                       open O
                       val bits = toBits v
                    in
                       alts (fn n => wr word8 (toW8 (bits ~>> n))) op >>
                    end,
            sz = SOME ((n + 7) div 8)}
      end

      val word32 = bits Word32Ops.ops Iso.id

      fun bytesAsBits (Ops.R {bytesPerElem, toBytes, subArr, ...}) =
          P {rd = let
                open I
                fun lp (a, i) =
                    if i < bytesPerElem
                    then rd word8 >>= (fn b =>
                         (Word8Array.update (a, i, b)
                        ; lp (a, i+1)))
                    else return (subArr (a, 0))
             in
                return () >>= (fn () =>
                lp (Word8Array.array (bytesPerElem, 0w0), 0))
             end,
             wr = fn v => let
                        open O
                        val bytes = toBytes v
                        fun lp i =
                            if i < bytesPerElem
                            then wr word8 (Word8Vector.sub (bytes, i))
                                    >>= (fn () => lp (i+1))
                            else return ()
                     in
                        lp 0
                     end,
             sz = SOME bytesPerElem}

      val mkReal =
       fn Ops.R {isoBits = SOME isoBits,
                 bitsOps = bitsOps as Ops.W {wordSize, ...}, ...} =>
          sized wordSize (bits bitsOps isoBits)
        | packOps as Ops.R {bytesPerElem, ...} =>
          sized (bytesPerElem * 8) (bytesAsBits packOps)

      (* Encodes fixed size int as a size followed by little endian bytes. *)
      fun mkFixedInt (Ops.W {orb, <<, ~>>, isoWord8 = (toW8, fromW8),
                             isoWord8X = (_, fromW8X), ...})
                     (fromBitsX, toBits) =
          P {rd = let
                open I
                fun lp (1, s, w) =
                    rd word8 >>= (fn b =>
                    return (fromBitsX (fromW8X b << s orb w)))
                  | lp (n, s, w) =
                    rd word8 >>= (fn b =>
                    lp (n - 1, s + 0w8, fromW8 b << s orb w))
             in
                rd size >>= (fn 0 => return (fromBitsX (fromW8 0w0))
                              | n => lp (n, 0w0, fromW8 0w0))
             end,
             wr = let
                open O
                fun lp (n, w, wr') = let
                   val n = n+1
                   val b = toW8 w
                   val wr' = wr' >> wr word8 b
                in
                   if fromW8X b = w
                   then wr size n >> wr'
                   else lp (n, w ~>> 0w8, wr')
                end
             in
                fn i => case toBits i
                         of w => if w = fromW8 0w0
                                 then wr size 0
                                 else lp (0, w, return ())
             end,
             sz = SOME 4}

      val () = if LargeWord.wordSize < valOf FixedInt.precision
               then fail "LargeWord can't hold a FixedInt"
               else ()
      val fixedInt = mkFixedInt LargeWordOps.ops LargeWord.isoFixedIntX

      fun cyclic {readProxy, readBody, writeWhole, self} = let
         val (toDyn, fromDyn) =
             Dyn.new {eq = Arg.eq self, hash = Word32.toWord o Arg.hash self}
         open I
      in
         P {rd = rd size >>= (fn key => Map.get >>= (fn arr =>
                 if 0 = key
                 then Key.alloc >>= (fn key => readProxy >>= (fn proxy =>
                      (ResizableArray.update (arr, key-1, toDyn proxy)
                     ; readBody proxy)))
                 else return (fromDyn (ResizableArray.sub (arr, key-1))))),
            wr = fn v => let
                       val d = toDyn v
                       open O
                    in
                       Map.get >>= (fn mp =>
                       case HashMap.find mp d
                        of SOME key => wr size key
                         | NONE     => Key.alloc >>= (fn key =>
                                       (HashMap.insert mp (d, key)
                                      ; wr size 0 >> writeWhole v)))
                    end,
            sz = NONE}
      end

      fun share aT (P {rd = aR, wr = aW, ...}) = let
         val (toDyn, fromDyn) =
             Dyn.new {eq = Arg.eq aT, hash = Word32.toWord o Arg.hash aT}
         open I
      in
         P {rd = rd size >>= (fn key => Map.get >>= (fn arr =>
                 if 0 = key
                 then Key.alloc >>= (fn key => aR >>= (fn v =>
                      (ResizableArray.update (arr, key-1, toDyn v)
                     ; return v)))
                 else return (fromDyn (ResizableArray.sub (arr, key-1))))),
            wr = fn v => let
                       val d = toDyn v
                       open O
                    in
                       Map.get >>= (fn mp =>
                       case HashMap.find mp d
                        of SOME key => wr size key
                         | NONE     => wr size 0 >> Key.alloc >>= (fn key =>
                                       aW v >>= (fn () =>
                                       (if isSome (HashMap.find mp d)
                                        then ()
                                        else HashMap.insert mp (d, key)
                                      ; return ()))))
                    end,
            sz = SOME 5}
      end

      fun mkSeq (Ops.S {length, toSlice, getItem, fromList, ...})
                (P {rd = aR, wr = aW, ...}) =
          P {rd = let
                open I
                fun lp (0, es) = return (fromList (rev es))
                  | lp (n, es) = aR >>= (fn e => lp (n-1, e::es))
             in
                rd size >>= lp /> []
             end,
             wr = let
                open O
                fun lp sl =
                    case getItem sl
                     of NONE         => return ()
                      | SOME (e, sl) => aW e >>= (fn () => lp sl)
             in
                fn seq => wr size (length seq) >>= (fn () =>
                          lp (toSlice seq))
             end,
             sz = NONE : OptInt.t}

      val string = share (Arg.Open.string ()) (mkSeq StringOps.ops char)

      val (h2n, n2h) = swap Word8.isoInt <--> Char.hexDigitIsoInt
      local
         fun makePos8 i =
             i + IntInf.<<
                    (1,
                     Word.andb
                        (Word.fromInt (IntInf.log2 (IntInf.notb i)) + 0w9,
                         Word.~ 0w8))
      in
         fun i2h i =
             if i < 0
             then if i = ~1 then "ff" else IntInf.fmt StringCvt.HEX (makePos8 i)
             else let
                   val s = IntInf.fmt StringCvt.HEX i
                   val (t, f) = if Int.isOdd (String.size s)
                                then ("0", "0")
                                else ("00", "")
                in
                   (if 0w8 <= h2n (String.sub (s, 0)) then t else f) ^ s
                end
      end
      fun h2i h =
          case valOf (StringCvt.scanString (IntInf.scan StringCvt.HEX) h)
           of i =>
              if 0w8 <= h2n (String.sub (h, 0))
              then i - IntInf.<< (1, Word.fromInt (IntInf.log2 i + 1))
              else i

      val intInf =
          P {wr = let
                open O
                fun lp (_, 0) = return ()
                  | lp (s, i) =
                    case i - 1 of i => pl (s, i, h2n (String.sub (s, i)))
                and pl (_, 0, b) = wr word8 b
                  | pl (s, i, b) = let
                       val i = i - 1
                    in
                       wr word8 (b + Word8.<< (h2n (String.sub (s, i)), 0w4))
                          >>= (fn () => lp (s, i))
                    end
             in
                fn i => if 0 = i then wr size 0 else let
                           val s = i2h i
                           val n = String.length s
                        in
                           wr size (Int.quot (n, 2)) >>= (fn () => lp (s, n))
                        end
             end,
             rd = let
                open I
                fun lp (cs, 0) = return (h2i (implode cs))
                  | lp (cs, n) =
                    rd word8 >>= (fn b =>
                    lp (n2h (Word8.>> (b, 0w4))::
                        n2h (Word8.andb (b, 0wxF))::cs, n-1))
             in
                rd size >>= (fn 0 => return 0 | n => lp ([], n))
             end,
             sz = SOME 4}

      val exns : {rd : String.t -> Exn.t I.monad Option.t,
                  wr : Exn.t -> Unit.t O.monad Option.t} Buffer.t =
          Buffer.new ()
      fun regExn c (P {rd = aR, wr = aW, ...}) (a2e, e2a) = let
         val c = Generics.Con.toString c
         val eR = I.map a2e aR
      in
         (Buffer.push exns)
            {rd = fn c' => if c' = c then SOME eR else NONE,
             wr = Option.map (fn a => O.>> (wr string c, aW a)) o e2a}
      end

      structure PickleRep = LayerRep
        (open Arg
         type 'a t = 'a t and 'a s = 'a s and ('a, 'k) p = 'a t)

      open PickleRep.This

      structure Pickle = struct
         structure P = O and U = I

         type 'a t = {pickler : 'a -> Unit.t P.monad,
                      unpickler : 'a U.monad}

         fun getPU t =
             case getT t of P {rd, wr, ...} => {pickler = wr, unpickler = rd}
         fun setPU {pickler, unpickler} =
             mapT (fn P {sz, ...} => P {rd = unpickler, wr = pickler, sz = sz})
         fun mapPU f t = setPU (f (getPU t)) t

         exception TypeMismatch

         fun withTypeHash t = let
            val key = Arg.typeHash t
         in
            mapPU (fn {pickler, unpickler} =>
                      {pickler = let
                          open P
                       in
                          fn v => wr word32 key >>= (fn () => pickler v)
                       end,
                       unpickler = let
                          open U
                       in
                          rd word32 >>= (fn key' =>
                          if key' <> key
                          then raise TypeMismatch
                          else unpickler)
                       end}) t
         end

         datatype 'a v = IN of Int.t -> 'a U.monad

         exception Version of Int.t

         fun check i = if i < 0 then raise Size else ()

         fun version iOfT t fromT =
             (check iOfT
            ; case U.map fromT (#unpickler (getPU t))
               of u => Fold.mapSt (fn IN other =>
                                      IN (fn i => if i = iOfT
                                                  then u
                                                  else other i)))

         fun versioned ? =
             Fold.wrap
                (IN (Exn.throw o Version),
                 fn IN other => fn iOfT => fn t =>
                    (check iOfT
                   ; case getPU t
                      of {pickler, unpickler} =>
                         setPU {pickler = let
                                   open P
                                in
                                   fn v => wr size iOfT >>= (fn () => pickler v)
                                end,
                                unpickler = let
                                   open U
                                in
                                   rd size >>= (fn i =>
                                   if i = iOfT then unpickler else other i)
                                end}
                               t)) ?
      end

      fun pickler aT =
          case wr (getT aT)
           of aW => fn a => fn b => fn c =>
              O.run (ref 0, HashMap.new {eq = Dyn.eq, hash = Dyn.hash}) aW a b c
      fun unpickler aT =
          case rd (getT aT)
           of aR => fn cR => fn s => I.run (ResizableArray.new ()) aR cR s

      fun pickle t =
          case pickler t (IOSMonad.fromPutter (uncurry Buffer.push))
           of aP => fn a => Buffer.toWord8Vector o #2 o aP a |< Buffer.new ()
      fun unpickle t =
          Pair.fst o unpickler t (IOSMonad.fromReader Word8VectorSequence.get) o
          Word8VectorSequence.full

      structure Open = LayerDepCases
        (fun iso bT aIb = let
            val bP = getT bT
            val aP = iso' bP aIb
         in
            if case sz bP of NONE => true | SOME n => 8 < n
            then share (Arg.Open.iso (const (const ())) bT aIb) aP
            else aP
         end

         fun isoProduct bP = iso' (getP bP)

         fun isoSum bS (a2b, b2a) =
             case getS bS
              of S {rd, wr, sz} =>
                 S {rd = fn i0 => fn i => I.map b2a (rd i0 i),
                    wr = fn i0 => wr i0 o a2b,
                    sz = sz}

         fun lT *` rT = let
            val P {rd = lR, wr = lW, sz = lS} = getP lT
            val P {rd = rR, wr = rW, sz = rS} = getP rT
         in
            P {rd = let open I in lR >< rR end,
               wr = let open O in fn l & r => lW l >> rW r end,
               sz = OptInt.+ (lS, rS)}
         end

         val T      = getT
         fun R _    = getT
         val tuple  = getP
         val record = getP

         fun lT +` rT = let
            val lN = Arg.numAlts lT
            val S {rd = lR, wr = lW, sz = lS} = getS lT
            val S {rd = rR, wr = rW, sz = rS} = getS rT
         in
            S {rd = fn l0 => let
                          val r0 = l0+lN
                          val lR = lR l0
                          val rR = rR r0
                       in
                          fn i => if i < r0
                                  then I.map INL (lR i)
                                  else I.map INR (rR i)
                       end,
               wr = fn l0 => Sum.sum (lW l0, rW (l0+lN)),
               sz = OptInt.+ (lS, rS)}
         end
         val unit = unit
         fun C0 _ = S {rd = const (const (I.return ())),
                       wr = fn i0 => const (i0, id),
                       sz = SOME 0}
         fun C1 _ aT =
             case getT aT
              of P {rd, wr, sz} =>
                 S {rd = const (const rd),
                    wr = fn i0 => fn v => (i0, fn t => O.>> (t, wr v)),
                    sz = sz}
         fun data aS = let
            val n = Arg.numAlts aS
            val tag =
                if      n <= 1   then intAs0
                else if n <= 256 then intAs8
                else                  size
            val S {rd = aR, wr = aW, sz = aS} = getS aS
            val aR = aR 0
            open I
         in
            P {rd = rd tag >>= (fn i =>
                    if i < n then aR i else fail "Corrupted pickle"),
               wr = (fn (i, m) => m (wr tag i)) o aW 0,
               sz = let open OptInt in aS div SOME n + sz tag end}
         end

         fun Y ? = let open Tie in iso (I.Y *` function *` id NONE) end
                      (fn P {rd, wr, sz} => rd & wr & sz,
                       fn rd & wr & sz => P {rd = rd, wr = wr, sz = sz}) ?

         fun op --> _ = fake "Pickle.--> unsupported"

         fun refc aT =
             case getT aT
              of P {rd, wr, ...} =>
                 cyclic {readProxy = I.thunk (fn () => ref (Arg.some aT)),
                         readBody = fn r => I.map (fn v => (r := v ; r)) rd,
                         writeWhole = wr o !,
                         self = Arg.Open.refc ignore aT}

         fun array aT =
             case getT aT
              of P {rd = aR, wr = aW, ...} =>
                 cyclic {readProxy =
                            I.map (fn n => Array.array (n, Arg.some aT))
                                  (rd size),
                         readBody = fn a => let
                            open I
                            fun lp i = if i = Array.length a
                                       then return a
                                       else aR >>= (fn e =>
                                            (Array.update (a, i, e)
                                           ; lp (i+1)))
                         in
                            lp 0
                         end,
                         writeWhole = fn a => let
                            open O
                            fun lp i =
                                if i = Array.length a
                                then return ()
                                else aW (Array.sub (a, i)) >>= (fn () =>
                                     lp (i+1))
                         in
                            wr size (Array.length a) >>= (fn () => lp 0)
                         end,
                         self = Arg.Open.array ignore aT}

         fun list aT =
             share (Arg.Open.list ignore aT) (mkSeq ListOps.ops (getT aT))

         fun vector aT =
             share (Arg.Open.vector ignore aT) (mkSeq VectorOps.ops (getT aT))

         val exn : Exn.t t =
             P {rd = let
                   open I
                in
                   rd string >>= (fn s =>
                   case Buffer.findSome (pass s o #rd) exns
                    of NONE   => fails ["Unregistered exn constructor: ", s]
                     | SOME r => r)
                end,
                wr = fn e => case Buffer.findSome (pass e o #wr) exns
                              of NONE   => GenericsUtil.failExn e
                               | SOME r => r,
                sz = NONE}
         fun regExn0 c (e, p) = regExn c unit (const e, p)
         fun regExn1 c aT = regExn c (getT aT)

         val fixedInt = fixedInt
         val largeInt = if isSome LargeInt.precision
                        then iso' fixedInt (swap FixedInt.isoLarge)
                        else intInf

         val char = char
         val bool = iso' word8 (swap Word8.isoInt <--> Bool.isoInt)
         val int =
             if case Int.precision
                 of NONE => false
                  | SOME n => n <= Word.wordSize
             then mkFixedInt WordOps.ops Word.isoIntX
             else if isSome Int.precision
             then iso' fixedInt Int.isoFixedInt
             else iso' largeInt Int.isoLargeInt
         val real = mkReal RealOps.ops
         val string = string
         val word = mkFixedInt WordOps.ops Iso.id

         val largeReal = mkReal LargeRealOps.ops
         val largeWord = mkFixedInt LargeWordOps.ops Iso.id

         val word8  = word8
         val word32 = word32
(*
         val word64 = bits false Word64Ops.ops Iso.id
*)

         fun hole () = P {rd = I.thunk undefined, wr = undefined, sz = NONE}

         open Arg PickleRep)
   end
in
   Result :> PICKLE_CASES
      where type ('a,     'x) Open.Rep.t = ('a,     'x) Result.Open.Rep.t
      where type ('a,     'x) Open.Rep.s = ('a,     'x) Result.Open.Rep.s
      where type ('a, 'k, 'x) Open.Rep.p = ('a, 'k, 'x) Result.Open.Rep.p
end
