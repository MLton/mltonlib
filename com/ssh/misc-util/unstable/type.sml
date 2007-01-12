(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(*
 * An implementation of the {TYPE} signature done by combining some of the
 * utility implementations of the {TYPE} signature.
 *)

structure Type :>
   sig
      include TYPE

      (** == STRUCTURAL TYPE-INDEXED VALUES == *)

      include ARBITRARY
      include COMPARE
      include EQ

      (** == NOMINAL TYPE-INDEXED VALUES == *)

      include SHOW

      (* Sharing constraints *)

      sharing type t
                 = arbitrary_t
                 = compare_t
                 = eq_t
                 = show_t
      sharing type s
                 = show_s
      sharing type p
                 = show_p
   end = struct
      structure Type =
         TypePair
            (structure A = Show
             structure B =
                StructuralTypeToType
                   (StructuralTypePair
                       (structure A = Arbitrary
                        structure B =
                           StructuralTypePair
                              (structure A = Compare
                               structure B = Eq))))

      structure T :
         sig
            type 'a t
            type 'a s
            type ('a, 'k) p
         end = Type

      local
         open Lift
      in
         val A = A
         val B = B
         val op ^ = op ^
      end

      structure Arbitrary = LiftArbitrary (open Arbitrary T fun lift () = B^A)
      structure Compare   = LiftCompare   (open Compare   T fun lift () = B^B^A)
      structure Eq        = LiftEq        (open Eq        T fun lift () = B^B^B)

      structure Show = LiftShow (open Show T fun liftT () = A)

      open Type
           Arbitrary
           Compare
           Eq
           Show
   end

(**
 * Here we extend the Type module with type-indices for some standard
 * types and type constructors as well as implement some utilities.
 *)
structure Type =
   struct
      open TypeSupport Type

      (* Convenience functions for making constructors and labels.  Use
       * these only for defining monomorphic type-indices.
       *)
      fun C0' n = C0 (C n)
      fun C1' n = C1 (C n)
      fun R' n = R (L n)

      (* Convenience functions for registering exceptions. *)
      fun regExn0 e p n = regExn (C0' n) (const e, p)
      fun regExn1 e p n t = regExn (C1' n t) (e, p)

      (* Convenience functions for defining small tuples. *)
      local
         fun mk t = iso (tuple t)
      in
         fun tuple2 (a, b) = mk (T a *` T b) Product.isoTuple2
         fun tuple3 (a, b, c) = mk (T a *` T b *` T c) Product.isoTuple3
         fun tuple4 (a, b, c, d) = mk (T a *` T b *` T c *` T d) Product.isoTuple4
      end

      (* Type-indices for some standard types. *)
      local
         fun mk precision int' large' =
             if isSome Int.precision andalso
                valOf precision <= valOf Int.precision then
                iso int int'
             else
                iso largeInt large'
      in
         (* Warning: The following encodings of sized integer types are
          *    not optimal for serialization.  (They do work, however.)
          *    For serialization, one should encode sized integer types
          *    in terms of the corresponding sized word types.
          *)
         val int8  = mk Int8.precision  Int8.isoInt  Int8.isoLarge
         val int16 = mk Int16.precision Int16.isoInt Int16.isoLarge
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

      structure OS' =
         struct
            val syserror = iso string (OS.errorName, valOf o OS.syserror)
         end

      (* Type-indices for some util library types. *)
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

      (* Abbreviations for type-indices. *)
      fun sq a = tuple2 (Sq.mk a)
      fun uop a = a --> a
      fun bop a = sq a --> a
   end

val () =
    let
       open IEEEReal OS OS.IO OS.Path Time Type
       val s = SOME
       val n = NONE
       val su = SOME ()
       val syserr = tuple2 (string, option OS'.syserror)
    in
       (* Handlers for (most if not all) standard exceptions: *)
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
     ; regExn1 SysErr     (fn SysErr   ? => s? | _ => n) "OS.SysErr" syserr
       (* Handlers for some util library exceptions: *)
     ; regExn0 Sum.Sum    (fn Sum.Sum    => su | _ => n) "Sum"
     ; regExn0 Fix.Fix    (fn Fix.Fix    => su | _ => n) "Fix"
    end
