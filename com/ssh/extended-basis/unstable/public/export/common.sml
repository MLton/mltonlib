(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 * Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Implementation independent extensions == *)

(** === Concept Signatures === *)

signature BITWISE = BITWISE
signature BOUNDED = BOUNDED
signature BOUNDED_CORE = BOUNDED_CORE
signature CASED = CASED
signature CFUNC = CFUNC
signature CSTRINGABLE = CSTRINGABLE
signature CSTRINGABLE_CORE = CSTRINGABLE_CORE
signature EMPTY = EMPTY
signature EQUALITY = EQUALITY
signature EQUALITY_CORE = EQUALITY_CORE
signature ETAEXP' = ETAEXP'
signature FLAGS = FLAGS
signature FORMATTABLE = FORMATTABLE
signature FORMATTABLE_and_SCANNABLE = FORMATTABLE_and_SCANNABLE
signature FORMATTABLE_and_SCANNABLE_FROM_FORMAT =
          FORMATTABLE_and_SCANNABLE_FROM_FORMAT
signature FUNC = FUNC
signature INTABLE = INTABLE
signature INTABLE_X = INTABLE_X
signature LARGEABLE = LARGEABLE
signature LARGEABLE_X = LARGEABLE_X
signature MAYBE_BOUNDED = MAYBE_BOUNDED
signature MAYBE_BOUNDED_CORE = MAYBE_BOUNDED_CORE
signature MONAD = MONAD
signature MONADP = MONADP
signature MONADP_CORE = MONADP_CORE
signature MONADP_EX = MONADP_EX
signature MONADP_STATE = MONADP_STATE
signature MONAD_CORE = MONAD_CORE
signature MONAD_EX = MONAD_EX
signature MONAD_STATE = MONAD_STATE
signature MONAD_WS = MONAD_WS
signature ORDERED = ORDERED
signature ORDERED_CORE = ORDERED_CORE
signature SCANNABLE = SCANNABLE
signature SCANNABLE_CORE = SCANNABLE_CORE
signature SCANNABLE_FROM_FORMAT = SCANNABLE_FROM_FORMAT
signature SCANNABLE_FROM_FORMAT_CORE = SCANNABLE_FROM_FORMAT_CORE
signature SHIFTABLE = SHIFTABLE
signature SIGNED = SIGNED
signature STRINGABLE = STRINGABLE
signature STRINGABLE_CORE = STRINGABLE_CORE
signature T = T
signature T'1 = T'1
signature T'2 = T'2
signature WORDABLE = WORDABLE
signature WORDABLE_X = WORDABLE_X

(** === Module Signatures === *)

signature ARRAY = ARRAY
signature ARRAY_SLICE = ARRAY_SLICE
signature BASIC = BASIC
signature BIN_FN = BIN_FN
signature BIN_OP = BIN_OP
signature BIN_PR = BIN_PR
signature BOOL = BOOL
signature BUFFER = BUFFER
signature CHAR = CHAR
signature CMP = CMP
signature CONTRACT = CONTRACT
signature CPS = CPS
signature CVT = CVT
signature EFFECT = EFFECT
signature EMB = EMB
signature EXIT = EXIT
signature EXN = EXN
signature FIX = FIX
signature FN = FN
signature FOLD = FOLD
signature FRU = FRU
signature ID = ID
signature INTEGER = INTEGER
signature INT_INF = INT_INF
signature IOS_MONAD = IOS_MONAD
signature ISO = ISO
signature ITER = ITER
signature LAZY = LAZY
signature LIST = LIST
signature MONO_ARRAY = MONO_ARRAY
signature MONO_ARRAY_SLICE = MONO_ARRAY_SLICE
signature MONO_VECTOR = MONO_VECTOR
signature MONO_VECTOR_SLICE = MONO_VECTOR_SLICE
signature OPTION = OPTION
signature ORDER = ORDER
signature OS = OS
signature OS_FILE_SYS = OS_FILE_SYS
signature PAIR = PAIR
signature PHANTOM = PHANTOM
signature PRODUCT = PRODUCT
signature PRODUCT_TYPE = PRODUCT_TYPE
signature READER = READER
signature REAL = REAL
signature REF = REF
signature RESIZABLE_ARRAY = RESIZABLE_ARRAY
signature SHIFT_OP = SHIFT_OP
signature SQ = SQ
signature STATIC_SUM = STATIC_SUM
signature STREAM = STREAM
signature STRING = STRING
signature SUBSTRING = SUBSTRING
signature SUM = SUM
signature TEXT = TEXT
signature TEXT_IO = TEXT_IO
signature THUNK = THUNK
signature TIE = TIE
signature TIME = TIME
signature UNIT = UNIT
signature UNIV = UNIV
signature UN_OP = UN_OP
signature UN_PR = UN_PR
signature VECTOR = VECTOR
signature VECTOR_SLICE = VECTOR_SLICE
signature VOID = VOID
signature WITH = WITH
signature WORD = WORD
signature WRITER = WRITER

(** === Modules === *)

(*
 * These structures are mentioned separately here, rather than in the
 * below list sorted alphabetically, because otherwise SML/NJ (v110.66)
 * barfs.
 *)
structure Sq : SQ = Sq
structure Sum : SUM = Sum
structure Thunk : THUNK = Thunk
structure Time : TIME = Time
structure UnOp : UN_OP = UnOp
structure UnPr : UN_PR = UnPr
structure Univ : UNIV = Univ
structure Vector : VECTOR = Vector
structure With : WITH = With
structure Writer : WRITER = Writer

structure Array : ARRAY = Array
structure ArraySlice : ARRAY_SLICE = ArraySlice
structure Basic : BASIC = Basic
structure BinFn : BIN_FN = BinFn
structure BinOp : BIN_OP = BinOp
structure BinPr : BIN_PR = BinPr
structure Bool : BOOL = Bool
structure Buffer : BUFFER = Buffer
structure CPS : CPS = CPS
structure Char : CHAR = Char
structure CharArray : MONO_ARRAY = CharArray
structure CharArraySlice : MONO_ARRAY_SLICE = CharArraySlice
structure CharVector : MONO_VECTOR = CharVector
structure CharVectorSlice : MONO_VECTOR_SLICE = CharVectorSlice
structure Cmp : CMP = Cmp
structure Contract : CONTRACT = Contract
structure Cvt : CVT = Cvt
structure Effect : EFFECT = Effect
structure Emb : EMB = Emb
structure Exit : EXIT = Exit
structure Exn : EXN = Exn
structure FRU : FRU = FRU
structure Fix : FIX = Fix
structure FixedInt : INTEGER = FixedInt
structure Fn : FN = Fn
structure Fold : FOLD = Fold
structure IOSMonad : IOS_MONAD = IOSMonad
structure Int : INTEGER = Int
structure Iso : ISO = Iso
structure Iter : ITER = Iter
structure LargeInt : INTEGER = LargeInt
structure LargeReal : REAL = LargeReal
structure LargeWord : WORD = LargeWord
structure Lazy : LAZY = Lazy
structure List : LIST = List
structure OS : OS = OS
structure Option : OPTION = Option
structure Order : ORDER = Order
structure Pair : PAIR = Pair
structure Phantom : PHANTOM = Phantom
structure Position : INTEGER = Position
structure Product : PRODUCT = Product
structure Reader : READER = Reader
structure Real : REAL = Real
structure Ref : REF where type 'a t = 'a ref = Ref
structure ResizableArray : RESIZABLE_ARRAY = ResizableArray
structure ShiftOp : SHIFT_OP = ShiftOp
structure StaticSum : STATIC_SUM = StaticSum
structure Stream : STREAM = Stream
structure String : STRING = String
structure Substring : SUBSTRING = Substring
structure Text : TEXT = Text
structure TextIO : TEXT_IO = TextIO
structure Tie : TIE = Tie
structure Unit : UNIT = Unit
structure VectorSlice : VECTOR_SLICE = VectorSlice
structure Void : VOID = Void
structure Word : WORD = Word
structure Word8 : WORD = Word8
structure Word8Array : MONO_ARRAY = Word8Array
structure Word8ArraySlice : MONO_ARRAY_SLICE = Word8ArraySlice
structure Word8Vector : MONO_VECTOR = Word8Vector
structure Word8VectorSlice : MONO_VECTOR_SLICE = Word8VectorSlice

(*
 * These structures are mentioned separately here, rather than in the
 * above list sorted alphabetically, because otherwise SML/NJ (v110.68)
 * barfs.
 *)
structure Id : ID = Id

(** === Functors === *)

functor MkBounded (C : BOUNDED_CORE) : BOUNDED = MkBounded (C)
functor MkCStringable (C : CSTRINGABLE_CORE) : CSTRINGABLE = MkCStringable (C)
functor MkEquality (C : EQUALITY_CORE) : EQUALITY = MkEquality (C)
functor MkMonad (C : MONAD_CORE) : MONAD = MkMonad (C)
functor MkMonadP (C : MONADP_CORE) : MONADP = MkMonadP (C)
functor MkOrdered (C : ORDERED_CORE) : ORDERED = MkOrdered (C)
functor MkScannable (C : SCANNABLE_CORE) : SCANNABLE = MkScannable (C)
functor MkStringable (C : STRINGABLE_CORE) : STRINGABLE = MkStringable (C)
functor MkWordFlags (C : WORD) : FLAGS = MkWordFlags (C)
