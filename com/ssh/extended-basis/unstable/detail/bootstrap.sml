(* Copyright (C) 2006-2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* Minimal modules for bootstrapping. *)

structure Exn = struct type t = exn end
structure Fn = struct type ('a, 'b) t = 'a -> 'b end
structure Unit = struct type t = unit end
structure Bool = struct open Bool type t = bool end
structure Char = struct open Char type t = char end
structure Option = struct open Option type 'a t = 'a option end
structure String = struct open String type t = string end
structure Int = struct open Int type t = int end
structure LargeInt = struct open LargeInt type t = int end
structure Word = struct open Word type t = word end
structure LargeWord = struct open LargeWord type t = word end
structure LargeReal = struct open LargeReal type t = real end
structure Word8Vector = struct open Word8Vector type t = vector end
structure Array = struct open Array type 'a t = 'a array end
structure ArraySlice = struct open ArraySlice type 'a t = 'a slice end
structure Vector = struct open Vector type 'a t = 'a vector end
structure VectorSlice = struct open VectorSlice type 'a t = 'a slice end
structure List = struct open List type 'a t = 'a list end
structure Effect = struct type 'a t = 'a -> Unit.t end
structure Order = struct datatype t = datatype order end
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
   type('a, 'b) t = ('a, 'b) sum
end
structure Sq = struct type 'a t = 'a * 'a end
structure Thunk = struct type 'a t = Unit.t -> 'a end
structure UnOp = struct type 'a t = 'a -> 'a end
structure UnPr = struct type 'a t = 'a -> Bool.t end
structure Fix = struct type 'a t = 'a UnOp.t -> 'a end
structure Reader = struct type ('a, 'b) t = 'b -> ('a * 'b) Option.t end
structure Writer = struct type ('a, 'b) t = 'a * 'b -> 'b end
structure Cmp = struct type 'a t = 'a Sq.t -> Order.t end
structure BinOp = struct type 'a t = 'a Sq.t -> 'a end
structure BinPr = struct type 'a t = 'a Sq.t UnPr.t end
structure Emb = struct type ('a, 'b) t = ('a -> 'b) * ('b -> 'a Option.t) end
structure Iso = struct type ('a, 'b) t = ('a -> 'b) * ('b -> 'a) end
structure ShiftOp = struct type 'a t = 'a * Word.t -> 'a end
structure BinFn = struct type ('a, 'b) t = 'a Sq.t -> 'b end
