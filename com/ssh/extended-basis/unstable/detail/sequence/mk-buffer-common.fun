(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor MkBufferCommon (type 'a elem
                        val inj : 'a -> 'a elem
                        val prj : 'a elem -> 'a
                        val any : 'a -> 'a elem) = struct
   structure A=Array and AS=ArraySlice and V=Vector and VS=VectorSlice
         and L=List
   datatype 'a t = T of {array : 'a elem A.t Ref.t, length : Int.t Ref.t}

   fun the s (T r) = s r
   fun get s = ! o the s
   fun set s t v = the s t := v

   fun array ? = get#array ?
   fun length ? = get#length ?

   fun isEmpty t = 0 = length t

   fun asub a i = A.sub (a, i)

   fun chk t i = if length t <= i then raise Subscript else ()

   fun sub (t, i) = (chk t i ; prj (asub (array t) i))
   fun update (t, i, v) = (chk t i ; A.update (array t, i, inj v))

   fun new () = T {array = ref (A.empty ()), length = ref 0}
   fun duplicate t = let
      val n = length t
   in
      T {array = ref (A.tabulate (n, asub (array t))), length = ref n}
   end

   fun capacity t = A.length (array t)
   fun trim t = set#array t (A.tabulate (length t, asub (array t)))

   fun realloc fill t newCap = let
      val n = length t
      val a = array t
   in
      set#array t (A.tabulate (newCap, fn i => if i<n then asub a i else fill))
   end

   fun ensureCap filler b reqCap = let
      val cap = capacity b
   in
      if reqCap <= cap then () else realloc filler b (Int.max (reqCap, cap*2+1))
   end

   local
      fun mk sLength sAny sAppi sInj b s =
          case sLength s of
             0 => ()
           | n => let
                val oldLength = length b
                val newLength = oldLength + n
             in ensureCap (sAny s) b newLength
              ; sAppi let
                   val a = array b
                in
                   fn (i, v) => A.update (a, i+oldLength, sInj v)
                end s : Unit.t
              ; set#length b newLength
             end
      infixr />
      val op /> = Fn./>
   in
      fun push ? = mk (Fn.const 1) any (fn ef => fn v => ef (0, v)) inj ?
      fun pushArray ? = mk A.length (any o A.sub /> 0) A.appi inj ?
      fun pushArraySlice ? = mk AS.length (any o AS.sub /> 0) AS.appi inj ?
      fun pushBuffer b s = mk AS.length (AS.sub /> 0) AS.appi Fn.id b
                              (AS.slice (array s, 0, SOME (length s)))
      fun pushList ? = mk L.length (any o L.hd) L.appi inj ?
      fun pushVector ? = mk V.length (any o V.sub /> 0) V.appi inj ?
      fun pushVectorSlice ? = mk VS.length (any o VS.sub /> 0) VS.appi inj ?
   end

   local
      fun to tabulate t = tabulate (length t, prj o asub (array t))
   in
      fun toArray  ? = to  A.tabulate ?
      fun toList   ? = to  L.tabulate ?
      fun toVector ? = to  V.tabulate ?
      val toString      = to CharVector.tabulate
      val toCharArray   = to CharArray.tabulate
      val toWord8Array  = to Word8Array.tabulate
      val toWord8Vector = to Word8Vector.tabulate
   end

   fun findSome p b = let
      fun lp i = if i < length b
                 then case p (sub (b, i))
                       of NONE   => lp (i+1)
                        | result => result
                 else NONE
   in
      lp 0
   end
end
