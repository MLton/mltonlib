(* Copyright (C) 2006 SSH Communications Security, Helsinki, Finland
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Buffer :> BUFFER = struct
   structure A = Array and AS = ArraySlice and V = Vector and VS = VectorSlice
   datatype 'a t = IN of {length : int ref, data : 'a A.t ref}
   fun new () = IN {length = ref 0, data = ref (A.fromList [])}
   fun duplicate (IN {length, data}) =
       IN {length = ref (!length), data = ref (A.duplicate (!data))}
   fun length (IN {length, ...}) = !length
   fun data (IN {data, ...}) = !data
   fun sub (b, i) = if length b <= i then raise Subscript else A.sub (data b, i)
   local
      fun cap b = A.length (data b)
      fun decideCap c r = if r <= c then c else decideCap (2*c+1) r
   in
      fun ensureCap (b as IN {data, ...}) reqCap filler =
          if reqCap <= cap b then ()
          else let val oldData = !data
               in data := A.tabulate (decideCap (cap b) reqCap,
                                      fn i => if A.length oldData <= i then
                                                 filler
                                              else
                                                 A.sub (oldData, i))
               end
   end
   local
      fun mk sLength sSub sCopy (b as IN {length, data}, s) =
          case sLength s of
             0 => ()
           | n => let
                val newLength = !length + n
             in ensureCap b newLength (sSub (s, 0))
              ; sCopy {src = s, dst = !data, di = !length} : unit
              ; length := newLength
             end
   in
      fun push ? = mk (Fn.const 1) Pair.fst
                      (fn {src, dst, di} => A.update (dst, di, src)) ?
      fun pushArray ? = mk A.length A.sub A.copy ?
      fun pushArraySlice ? = mk AS.length AS.sub AS.copy ?
      fun pushVector ? = mk V.length V.sub A.copyVec ?
      fun pushVectorSlice ? = mk VS.length VS.sub AS.copyVec ?
   end
   local
      fun mk tabulate b = tabulate (length b, fn i => sub (b, i))
   in
      fun toArray  ? = mk A.tabulate ?
      fun toVector ? = mk V.tabulate ?
   end
end
