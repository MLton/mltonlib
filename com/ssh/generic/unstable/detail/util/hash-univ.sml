(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

signature HASH_UNIV = sig
   type t
   val new : {eq : 'a BinPr.t, hash : 'a -> Word.t} -> ('a, t) Iso.t
   val eq : t BinPr.t
   val hash : t -> Word.t
end

structure HashUniv :> HASH_UNIV = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   (* SML/NJ workaround --> *)

   datatype t = T of {value : Univ.t, hash : Word.t Lazy.t,
                      methods : {eq : Univ.t BinPr.t} Ref.t}
   fun new {eq, hash} = let
      val (to, from) = Univ.Iso.new ()
      val methods = ref {eq = BinPr.map from eq}
   in
      (fn value => T {value = to value,
                      hash = delay (fn () => hash value),
                      methods = methods},
       fn T r => from (#value r))
   end
   fun eq (T l, T r) = #methods l = #methods r
                       andalso #eq (! (#methods l)) (#value l, #value r)
   fun hash (T r) = force (#hash r)
end
