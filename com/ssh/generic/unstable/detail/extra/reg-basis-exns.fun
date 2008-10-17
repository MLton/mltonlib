(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor RegBasisExns (include CLOSED_CASES) = struct
   val () = let
      open Generics (*IEEEReal*) OS OS.IO OS.Path Time

      val n = NONE
      val s = SOME

      fun reg0 n e p = regExn0 (C n) (e, p)
      fun reg1 n t e p = regExn1 (C n) t (e, p)
   in
      (* Handlers for most standard exceptions: *)
      reg0 "Bind"               Bind         (fn Bind         => s() | _ => n)
    ; reg0 "Chr"                Chr          (fn Chr          => s() | _ => n)
    ; reg0 "Date.Date"          Date.Date    (fn Date.Date    => s() | _ => n)
    ; reg0 "Div"                Div          (fn Div          => s() | _ => n)
    ; reg0 "Domain"             Domain       (fn Domain       => s() | _ => n)
    ; reg0 "Empty"              Empty        (fn Empty        => s() | _ => n)
    ; reg0 "OS.Path.InvalidArc" InvalidArc   (fn InvalidArc   => s() | _ => n)
    ; reg0 "Match"              Match        (fn Match        => s() | _ => n)
    ; reg0 "Option"             Option       (fn Option       => s() | _ => n)
    ; reg0 "Overflow"           Overflow     (fn Overflow     => s() | _ => n)
    ; reg0 "OS.Path.Path"       Path         (fn Path         => s() | _ => n)
(*
    ; reg0 "OS.IO.Poll"         Poll         (fn Poll         => s() | _ => n)
*)
    ; reg0 "Size"               Size         (fn Size         => s() | _ => n)
    ; reg0 "Span"               Span         (fn Span         => s() | _ => n)
    ; reg0 "Subscript"          Subscript    (fn Subscript    => s() | _ => n)
    ; reg0 "Time.Time"          Time         (fn Time         => s() | _ => n)
(*
    ; reg0 "IEEEReal.Unordered" Unordered    (fn Unordered    => s() | _ => n)
*)
    ; reg1 "Fail" string        Fail         (fn Fail       ? => s ? | _ => n)
      (* Handlers for some extended-basis exceptions: *)
    ; reg0 "IOSMonad.EOS"       IOSMonad.EOS (fn IOSMonad.EOS => s() | _ => n)
    ; reg0 "Sum.Sum"            Sum.Sum      (fn Sum.Sum      => s() | _ => n)
    ; reg0 "Fix.Fix"            Fix.Fix      (fn Fix.Fix      => s() | _ => n)
   end
end
