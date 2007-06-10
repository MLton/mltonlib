(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor WithExtra (Arg : GENERIC) : GENERIC_EXTRA = struct
   (* <-- SML/NJ workaround *)
   open TopLevel
   infix  7 *`
   infix  6 +` |`
   infix  0 & &`
   infixr 0 -->
   (* SML/NJ workaround --> *)

   open Generics Arg

   fun C0' n = C0 (C n)
   fun C1' n = C1 (C n)
   fun R' n = R (L n)

   fun regExn0 e p n = regExn (C0' n) (const e, p)
   fun regExn1 e p n t = regExn (C1' n t) (e, p)

   local
      fun mk t = iso (tuple t)
   in
      fun tuple2 (a, b) = mk (T a *` T b) Product.isoTuple2
      fun tuple3 (a, b, c) = mk (T a *` T b *` T c) Product.isoTuple3
      fun tuple4 (a, b, c, d) = mk (T a *` T b *` T c *` T d) Product.isoTuple4
   end

   local
      fun mk precision int' large' =
          if isSome Int.precision andalso
             valOf precision <= valOf Int.precision then
             iso int int'
          else
             iso largeInt large'
   in
   (* val int8  = mk Int8.precision  Int8.isoInt  Int8.isoLarge
      (* Int8 not provided by SML/NJ *) *)
   (* val int16 = mk Int16.precision Int16.isoInt Int16.isoLarge
      (* Int16 not provided by SML/NJ *) *)
      val int32 = mk Int32.precision Int32.isoInt Int32.isoLarge
      val int64 = mk Int64.precision Int64.isoInt Int64.isoLarge
   end

   local
      val none = C "NONE"
      val some = C "SOME"
   in
      fun option a =
          iso (data (C0 none +` C1 some a))
              (fn NONE => INL () | SOME a => INR a,
               fn INL () => NONE | INR a => SOME a)
   end

   val order =
       iso (data (C0' "LESS" +` C0' "EQUAL" +` C0' "GREATER"))
           (fn LESS => INL (INL ())
             | EQUAL => INL (INR ())
             | GREATER => INR (),
            fn INL (INL ()) => LESS
             | INL (INR ()) => EQUAL
             | INR () => GREATER)

   local
      val et = C "&"
   in
      fun a &` b = data (C1 et (tuple (T a *` T b)))
   end

   local
      val inl = C "INL"
      val inr = C "INR"
   in
      fun a |` b = data (C1 inl a +` C1 inr b)
   end

   fun sq a = tuple2 (Sq.mk a)
   fun uop a = a --> a
   fun bop a = sq a --> a

   val () = let
      open IEEEReal OS OS.IO OS.Path Time
      val s = SOME
      val n = NONE
      val su = SOME ()
   in
      (* Handlers for most standard exceptions: *)
      regExn0 Bind       (fn Bind       => su | _ => n) "Bind"
    ; regExn0 Chr        (fn Chr        => su | _ => n) "Chr"
    ; regExn0 Date.Date  (fn Date.Date  => su | _ => n) "Date.Date"
    ; regExn0 Div        (fn Div        => su | _ => n) "Div"
    ; regExn0 Domain     (fn Domain     => su | _ => n) "Domain"
    ; regExn0 Empty      (fn Empty      => su | _ => n) "Empty"
    ; regExn0 InvalidArc (fn InvalidArc => su | _ => n) "OS.Path.InvalidArc"
    ; regExn0 Match      (fn Match      => su | _ => n) "Match"
    ; regExn0 Option     (fn Option     => su | _ => n) "Option"
    ; regExn0 Overflow   (fn Overflow   => su | _ => n) "Overflow"
    ; regExn0 Path       (fn Path       => su | _ => n) "OS.Path.Path"
    ; regExn0 Poll       (fn Poll       => su | _ => n) "OS.IO.Poll"
    ; regExn0 Size       (fn Size       => su | _ => n) "Size"
    ; regExn0 Span       (fn Span       => su | _ => n) "Span"
    ; regExn0 Subscript  (fn Subscript  => su | _ => n) "Subscript"
    ; regExn0 Time       (fn Time       => su | _ => n) "Time.Time"
    ; regExn0 Unordered  (fn Unordered  => su | _ => n) "IEEEReal.Unordered"
    ; regExn1 Fail       (fn Fail     ? => s? | _ => n) "Fail"      string
      (* Handlers for some extended-basis exceptions: *)
    ; regExn0 Sum.Sum    (fn Sum.Sum    => su | _ => n) "Sum"
    ; regExn0 Fix.Fix    (fn Fix.Fix    => su | _ => n) "Fix"
   end
end
