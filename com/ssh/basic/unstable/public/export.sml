(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BIN_OP = BIN_OP
signature BIN_PR = BIN_PR
signature BUFFER = BUFFER
signature CMP = CMP
signature EFFECT = EFFECT
signature EXIT = EXIT
signature FIX = FIX
signature FN = FN
signature ISO = ISO
signature ORDER = ORDER
signature PAIR = PAIR
signature PRODUCT = PRODUCT
signature PRODUCT_TYPE = PRODUCT_TYPE
signature READER = READER
signature REF = REF
signature SQ = SQ
signature SUM = SUM
signature THUNK = THUNK
signature TIE = TIE
signature UNIV = UNIV
signature UN_OP = UN_OP
signature UN_PR = UN_PR
signature WRITER = WRITER

structure BinOp : BIN_OP = BinOp
structure BinPr : BIN_PR = BinPr
structure Buffer : BUFFER = Buffer
structure Cmp : CMP = Cmp
structure Effect : EFFECT = Effect
structure Exit : EXIT = Exit
structure Fix : FIX = Fix
structure Fn : FN = Fn
structure Iso : ISO = Iso
structure Order : ORDER = Order
structure Pair : PAIR = Pair
structure Product : PRODUCT = Product
structure Reader : READER = Reader
structure Ref : REF where type 'a t = 'a ref = Ref
structure Sq : SQ = Sq
structure Sum : SUM = Sum
structure Thunk : THUNK = Thunk
structure Tie : TIE = Tie
structure UnOp : UN_OP = UnOp
structure UnPr : UN_PR = UnPr
structure Univ : UNIV = Univ
structure Writer : WRITER = Writer
