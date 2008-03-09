(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithShrink (Arg : WITH_SHRINK_DOM) : SHRINK_CASES = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix 7 >> << *`
   infix 6 +`
   infix 4 orb
   infix 0 &
   (* SML/NJ workaround --> *)

   type e = Univ.t List.t
   datatype 'a t =
      IN of {kids : Unit.t Ref.t * e * 'a -> e,
             shrink : 'a -> 'a List.t}

   fun iso' (IN {kids, shrink}) (a2b, b2a) =
       IN {kids = fn (i, e, a) => kids (i, e, a2b a),
           shrink = map b2a o shrink o a2b}

   fun list' (IN {kids, shrink}) = let
      fun shrinkL []      = []
        | shrinkL (x::xs) =
          [xs] @
          map (fn x => x::xs) (shrink x) @
          map (fn xs => x::xs) (shrinkL xs)
   in
      IN {kids = fn (i, e, xs) => foldl (fn (x, e) => kids (i, e, x)) e xs,
          shrink = shrinkL}
   end

   val none =
       IN {kids = fn (_, e, _) => e,
           shrink = fn _ => []}

   fun mkInt (Ops.I {precision, isoInt = (_, fromInt), maxInt, +`, *`, div, mod,
                     ...}) =
       if isSome precision
       then IN {kids = fn (_, e, _) => e,
                shrink = fn i => let
                               val m = valOf maxInt div fromInt 2 +` fromInt 1
                               fun lp (d, is) = let
                                  val h = (i div d) div fromInt 2 *` d
                                  val l = i mod d
                                  val i' = h+`l
                               in
                                  if i' = i then is
                                  else if d = m then i'::is
                                  else lp (d *` fromInt 2, i'::is)
                               end
                            in
                               lp (fromInt 1, [])
                            end}
       else none

   fun mkWord (Ops.W {wordSize, <<, >>, orb, ...}) =
       IN {kids = fn (_, e, _) => e,
           shrink = fn w => let
                          fun lp (s, ws) =
                              if s = Word.fromInt wordSize then ws else let
                                 val h = (w >> (s + 0w1)) << s
                                 val s' = Word.fromInt wordSize - s
                                 val l = (w << s') >> s'
                                 val w' = h orb l
                              in
                                 if w' = w then ws else lp (s+0w1, w'::ws)
                              end
                       in
                          lp (0w0, [])
                       end}

   structure ShrinkRep = LayerRep' (open Arg type 'a t = 'a t)

   open ShrinkRep.This

   fun sortUniq aT = let
      val sizeOf = Arg.sizeOf aT
      val ord = Arg.ord aT
      fun uniq xs = let
         fun lp (ys, xs) =
             case xs
              of [] => ys
               | [(_ & x)] => x::ys
               | (s1 & x1)::(s2 & x2)::xs =>
                 if s1 = s2 andalso EQUAL = ord (x1, x2)
                 then lp (ys, (s2 & x2)::xs)
                 else lp (x1::ys, (s2 & x2)::xs)
      in
         rev (lp ([], xs))
      end
   in
      uniq o
      List.sort (Cmp.*` (Int.compare, ord)) o
      map (fn x => sizeOf x & x)
   end

   fun shrink aT =
       case getT aT
        of IN {shrink, ...} => sortUniq aT o shrink

   fun shrinkFix aT = let (* XXX suboptimal *)
      val shrink = shrink aT
      val sortUniq = sortUniq aT
      fun lp (toShrink, shrunken) = let
         val shrunken = sortUniq (toShrink @ shrunken)
         val toShrink = List.concatMap shrink toShrink
      in
         if null toShrink then shrunken else lp (toShrink, shrunken)
      end
   in
      fn x => lp (shrink x, [])
   end

   structure Open = LayerDepCases
     (fun iso        aT = iso' (getT aT)
      fun isoProduct aP = iso' (getP aP)
      fun isoSum     aS = iso' (getS aS)

      fun op *` (aP, bP) = let
         val IN aS = getP aP
         val IN bS = getP bP
      in
         IN {kids = fn (i, e, a & b) => #kids bS (i, #kids aS (i, e, a), b),
             shrink = fn a & b =>
                         map (fn a => a & b) (#shrink aS a) @
                         map (fn b => a & b) (#shrink bS b)}
      end
      val T      = getT
      fun R _    = getT
      val tuple  = getP
      val record = getP

      fun op +` (aS, bS) = let
         val IN aS = getS aS
         val IN bS = getS bS
      in
         IN {kids = fn (i, e, INL a) => #kids aS (i, e, a)
                     | (i, e, INR b) => #kids bS (i, e, b),
             shrink = fn INL a => map INL (#shrink aS a)
                       | INR b => map INR (#shrink bS b)}
      end
      val unit = none
      fun C0 _ = unit
      fun C1 _ = getT
      val data = getS

      fun Y ? = Tie.pure (fn () => let
         val i = ref ()
         val (to, from) = Univ.Iso.new ()
         val r = ref (raising Fix.Fix)
      in
         (IN {kids = fn (i', e, x) => if i = i' then to x :: e else e,
              shrink = fn x => !r x},
          fn IN {kids, shrink} => let
                fun shrinkT x = let
                   val ks = map from (kids (i, [], x))
                in
                   ks @ shrink x
                end
             in
                r := shrinkT
              ; IN {kids = kids, shrink = shrinkT}
             end)
      end) ?

      fun op --> _ = none

      val exn = none
      fun regExn0 _ _ = ()
      fun regExn1 _ _ _ = ()

      fun array  _ = none
      fun list aT = list' (getT aT)
      fun vector aT = iso' (list aT) Vector.isoList

      fun refc _ = none

      val fixedInt  = mkInt FixedIntOps.ops
      val largeInt  = mkInt LargeIntOps.ops

      val largeReal = none
      val largeWord = mkWord LargeWordOps.ops

      val bool   = none
      val char   = none
      val int    = mkInt IntOps.ops
      val real   = none
      val string = iso' (list' char) String.isoList
      val word   = mkWord WordOps.ops

      val word8  = mkWord Word8Ops.ops
      val word32 = mkWord Word32Ops.ops
(*
      val word64 = mkWord Word64Ops.ops
*)

      fun hole () = IN {kids = undefined, shrink = undefined}

      open Arg ShrinkRep)
end
