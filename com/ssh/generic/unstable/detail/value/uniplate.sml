(* Copyright (C) 2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(* TBD: Avoid redundantly querying/transforming substructures *)

functor WithUniplate (Arg : WITH_UNIPLATE_DOM) : UNIPLATE_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 7 >> << *`
   infix 6 +`
   infix 4 orb
   infix 0 &
   (* SML/NJ workaround --> *)

   type r = Unit.t Ref.t Option.t
   type 'a i = r * 'a Univ.Iso.t

   val dummy = (NONE, (undefined, undefined))

   type e = (HashUniv.t, Unit.t) HashMap.t
   type c = Univ.t List.t
   datatype 'a t =
      IN of 'a i * ((r * e) * c * 'a -> c) * ((r * e) * c * 'a -> 'a * c)

   val none = IN (dummy, fn (_, c, _) => c, fn (_, c, x) => (x, c))

   fun cyclic aT (IN (_, aKi, aKo)) = let
      val (to, _) = HashUniv.new {eq = op =, hash = Word32.toWord o Arg.hash aT}
   in
      IN (dummy,
          fn args as ((_, e), c, x) => let
                val xD = to x
             in
                if isSome (HashMap.find e xD) then c
                else (HashMap.insert e (xD, ()) ; aKi args)
             end,
          fn args as ((_, e), c, x) => let
                val xD = to x
             in
                if isSome (HashMap.find e xD) then (x, c)
                else (HashMap.insert e (xD, ()) ; aKo args)
             end)
   end

   fun op `*` (IN (_, aKi, aKo), IN (_, bKi, bKo)) =
       IN (dummy,
           fn (r, c, a & b) => aKi (r, bKi (r, c, b), a),
           fn (r, c, a & b) =>
              case aKo (r, c, a)
               of (a, c) =>
                  case bKo (r, c, b)
                   of (b, c) => (a & b, c))
   fun op `+` (IN (_, aKi, aKo), IN (_, bKi, bKo)) =
       IN (dummy,
           fn (r, c, INL a) => aKi (r, c, a)
            | (r, c, INR b) => bKi (r, c, b),
           fn (r, c, INL a) => Pair.map (INL, id) (aKo (r, c, a))
            | (r, c, INR b) => Pair.map (INR, id) (bKo (r, c, b)))
   fun iso' (IN (_, ki, ko)) (a2b, b2a) =
       IN (dummy,
           fn (r, c, a) => ki (r, c, a2b a),
           fn (r, c, a) => Pair.map (b2a, id) (ko (r, c, a2b a)))

   structure UniplateRep = LayerRep' (open Arg type 'a t = 'a t)

   open UniplateRep.This

   fun newMap () = HashMap.new {eq = HashUniv.eq, hash = HashUniv.hash}

   fun uniplate' aT =
       case getT aT
        of IN ((NONE, _), _, _) =>
           (fn x => ([], fn _ => x))
         | IN ((r, (to, from)), ki, ko) =>
           (fn x => (map from (ki ((r, newMap ()), [], x)),
                     fn xs => #1 (ko ((r, newMap ()), map to xs, x))))

   fun children t = #1 o uniplate' t

   fun holesC t =
       (fn (k, c) => let
              fun lp hs ys =
               fn []    => rev hs
                | x::xs =>
                  lp ((x, fn x => c (List.revAppend (ys, x::xs)))::hs) (x::ys) xs
           in
              lp [] [] k
           end) o
       uniplate' t
   fun holesU t x = let
      fun lp (x, f, ys) =
          foldl (fn ((x, c), ys) => lp (x, f o c, ys))
                ((x, f)::ys)
                (holesC t x)
   in
      rev (lp (x, id, []))
   end

   fun foldC t f s = foldl f s o children t
   fun foldU t f s x = foldC t (fn (x, s) => foldU t f s x) (f (x, s)) x

   local
      fun mk fold t zero op + one = fold t (fn (x, sum) => one x + sum) zero
   in
      fun reduceC ? = mk foldC ?
      fun reduceU ? = mk foldU ?
   end

   fun transformC t f = (fn (k, c) => c (map f k)) o uniplate' t
   fun transformU t f x = f (transformC t (transformU t f) x)

   fun para t f x = f x (map (para t f) (children t x))

   fun rewrite t f =
       transformU t (fn x => case f x of NONE => x | SOME x => rewrite t f x)
   fun universe t = rev o foldU t op :: []

   fun uniplate t =
       (fn (children, context) =>
           (children,
            context o (case length children
                        of n => fn children =>
                                   if n <> length children
                                   then fail "wrong number of children"
                                   else children))) o
       uniplate' t

   structure Open = LayerDepCases
     (fun iso        bT = iso' (getT bT)
      fun isoProduct bP = iso' (getP bP)
      fun isoSum     bS = iso' (getS bS)

      fun op *` (aP, bP) = op `*` (getP aP, getP bP)
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = op `+` (getS aS, getS bS)
      val unit = none
      fun C0 _ = unit
      fun C1 _ = getT
      val data = getS

      fun Y ? = Tie.pure (fn () => let
         val r = SOME (ref ())
         val iso as (to, from) = Univ.Iso.new ()
         val rKi = ref (raising Fix.Fix)
         fun ki' ? = !rKi ?
         val rKo = ref (raising Fix.Fix)
         fun ko' ? = !rKo ?
         val i = (r, iso)
      in
         (IN (i,
              fn args as ((r', _), c, x) =>
                 if r = r' then to x::c else ki' args,
              fn args as ((r', _), c, _) =>
                 if r = r'
                 then case c
                       of []   => fail "bug"
                        | x::c => (from x, c)
                 else ko' args),
          fn IN (_, ki, ko) => (rKi := ki ; rKo := ko ; IN (i, ki, ko)))
      end) ?

      fun op --> _ = none

      val exn = none
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun array aT =
          case getT aT
           of IN (_, aKi, aKo) =>
              cyclic (Arg.Open.array ignore aT)
                     (IN (dummy,
                          fn (r, c, s) =>
                             Array.foldr (fn (a, c) => aKi (r, c, a)) c s,
                          fn (r, c, s) => let
                                fun lp i c =
                                    if i = Array.length s
                                    then (s, c)
                                    else case aKo (r, c, Array.sub (s, i))
                                          of (x, c) =>
                                             (Array.update (s, i, x)
                                            ; lp (i+1) c)
                             in
                                lp 0 c
                             end))
      fun list aT =
          (Tie.fix Y)
             (fn aListT =>
                 iso' (op `+` (unit, op `*` (getT aT, aListT)))
                      (fn [] => INL () | x::xs => INR (x & xs),
                       fn INL () => [] | INR (x & xs) => x::xs))
      fun vector aT =
          case getT aT
           of (IN (_, aKi, aKo)) =>
              IN (dummy,
                  fn (r, c, s) =>
                     Vector.foldr (fn (a, c) => aKi (r, c, a)) c s,
                  fn (r, c, s) =>
                     Vector.unfoldi
                        (fn (i, c) => aKo (r, c, Vector.sub (s, i)))
                        (Vector.length s, c))

      fun refc aT =
          case getT aT
           of IN (_, aKi, aKo) =>
              cyclic (Arg.Open.refc ignore aT)
                     (IN (dummy,
                          fn (r, c, s) => aKi (r, c, !s),
                          fn (r, c, s) => case aKo (r, c, !s)
                                           of (x, c) => (s := x ; (s, c))))

      val fixedInt  = none
      val largeInt  = none

      val largeReal = none
      val largeWord = none

      val bool   = none
      val char   = none
      val int    = none
      val real   = none
      val string = none
      val word   = none

      val word8  = none
      val word32 = none
(*
      val word64 = none
*)

      fun hole () = IN (dummy, undefined, undefined)

      open Arg UniplateRep)
end
