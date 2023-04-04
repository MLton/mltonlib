(* Copyright (C) 2006-2008 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Minimal modules for bootstrapping. *)

structure Void = struct datatype t = T of t fun void (T t) = void t end
structure Exn = struct type t = exn end
structure Fn = struct type ('a, 'b) t = 'a -> 'b end
structure Unit = struct type t = unit fun compare ((), ()) = EQUAL end
structure Bool = struct
   open BasisBool
   type t = bool
   fun isFalse b = b = false
   fun isTrue b = b = true
end
structure Array = struct open BasisArray type 'a t = 'a array end
structure ArraySlice = struct open BasisArraySlice type 'a t = 'a slice end
structure Char = struct open BasisChar type t = char end
structure CharArray = struct open BasisCharArray type t = array end
structure CharArraySlice = struct open BasisCharArraySlice type t = slice end
structure CharVector = struct open BasisCharVector type t = vector end
structure CharVectorSlice = struct open BasisCharVectorSlice type t = slice end
structure Effect = struct type 'a t = 'a -> Unit.t end
structure FixedInt = struct open BasisFixedInt type t = int end
structure Int = struct open BasisInt type t = int end
structure Real = struct open BasisReal type t = real end
structure LargeInt = struct open BasisLargeInt type t = int end
structure LargeReal = struct open BasisLargeReal type t = real end
structure LargeWord = struct open BasisLargeWord type t = word end
structure List = struct open BasisList type 'a t = 'a list end
structure Option = struct open BasisOption type 'a t = 'a option end
structure Order = struct
   datatype order = datatype BasisGeneral.order
   type t = order
end
structure String = struct open BasisString type t = string end
structure Substring = struct open BasisSubstring type t = substring end
structure Vector = struct open BasisVector type 'a t = 'a vector end
structure VectorSlice = struct open BasisVectorSlice type 'a t = 'a slice end
structure Word = struct open BasisWord type t = word end
structure Word8 = struct open BasisWord8 type t = word end
structure Word8Array = struct open BasisWord8Array type t = array end
structure Word8ArraySlice = struct open BasisWord8ArraySlice type t = slice end
structure Word8Vector = struct open BasisWord8Vector type t = vector end
structure Word8VectorSlice = struct
   open BasisWord8VectorSlice
   type t = slice
end
structure Pair = struct
   type ('a, 'b) pair = 'a * 'b
   type ('a, 'b) t = ('a, 'b) pair
end
structure Product = struct
   datatype ('a, 'b) product = & of 'a * 'b
   type ('a, 'b) t = ('a, 'b) product
end
structure Ref = struct type 'a t = 'a ref end
structure Sum = struct
   datatype ('a, 'b) sum = INL of 'a | INR of 'b
   type ('a, 'b) t = ('a, 'b) sum
end
structure Sq = struct type 'a t = 'a * 'a end
structure Thunk = struct type 'a t = Unit.t -> 'a end
structure UnOp = struct type 'a t = 'a -> 'a end
structure UnPr = struct type 'a t = 'a -> Bool.t end
structure Fix = struct type 'a t = 'a UnOp.t -> 'a end
structure Reader = struct type ('a, 'b) t = 'b -> ('a * 'b) Option.t end
structure Writer = struct type ('a, 'b) t = 'a * 'b -> 'b end
structure Cmp = struct open Product type 'a t = 'a Sq.t -> Order.t end
structure BinOp = struct type 'a t = 'a Sq.t -> 'a end
structure BinPr = struct type 'a t = 'a Sq.t UnPr.t end
structure Emb = struct type ('a, 'b) t = ('a -> 'b) * ('b -> 'a Option.t) end
structure Iso = struct type ('a, 'b) t = ('a -> 'b) * ('b -> 'a) end
structure ShiftOp = struct type 'a t = 'a * Word.t -> 'a end
structure BinFn = struct type ('a, 'b) t = 'a Sq.t -> 'b end
structure IEEEReal = BasisIEEEReal
structure Time = struct open BasisTime type t = time end
structure CPS = struct type ('a, 'b) t = ('a -> 'b) -> 'b end
structure Id = struct type 'a t = 'a end
