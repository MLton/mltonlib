(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

structure Node :> NODE = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  4 <\
   infixr 4 />
   (* SML/NJ workaround --> *)

   datatype 'a t = T of 'a * 'a p
   withtype 'a p = 'a t Option.t Ref.t

   fun ptr () = ref NONE
   fun new v = T (v, ptr ())

   fun next (T (_, p)) = p
   fun value (T (v, _)) = v

   fun isEmpty p = isNone (!p)

   fun nonEmpty f p = case !p of NONE => raise Empty | SOME n => f n
   fun hd p = nonEmpty value p
   fun tl p = nonEmpty next p

   fun drop p = p := !(tl p)

   fun push p v = let
      val n = new v
   in
      next n := !p ; p := SOME n
   end

   fun pop p =
       case !p of
          NONE => NONE
        | SOME (T (v, p')) => (p := !p' ; SOME v)

   fun peek p =
       case !p of
          NONE => NONE
        | SOME (T (v, _)) => SOME v

   fun find c p =
       case !p of
          NONE => INL p
        | SOME (T (v, p')) => if c v then INR p else find c p'

   fun fold f s p =
       case !p of
          NONE => s
        | SOME (T (v, p)) => fold f (f (v, s)) p

   fun toList p = rev (fold op :: [] p)

   fun length p = fold (1 <\ op + o #2) 0 p

   fun filter c p =
       case !p of
          NONE => p
        | SOME (T (v, n)) => if c v then filter c n else (p := !n ; filter c p)

   fun appClear ef p =
       case !p of
          NONE => p
        | SOME (T (v, n)) => (ef v : unit ; p := !n ; appClear ef p)

   fun insert lt p v =
       case !p of
          NONE => push p v
        | SOME (T (x, p')) =>
          if lt (x, v) then insert lt p' v else push p v
end
