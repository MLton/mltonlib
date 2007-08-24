(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

functor RegBasisExns (Arg : CLOSED_CASES) = struct
   val () = let
      open Arg Generics IEEEReal OS OS.IO OS.Path Time

      local
         fun lift f a = SOME (f a) handle Match => NONE
      in
         fun regExn0' n e p = regExn0 (C n) (e, lift p)
         fun regExn1' n t e p = regExn1 (C n) t (e, lift p)
      end
   in
      (* Handlers for most standard exceptions: *)
      regExn0' "Bind"               Bind         (fn Bind         => ())
    ; regExn0' "Chr"                Chr          (fn Chr          => ())
    ; regExn0' "Date.Date"          Date.Date    (fn Date.Date    => ())
    ; regExn0' "Div"                Div          (fn Div          => ())
    ; regExn0' "Domain"             Domain       (fn Domain       => ())
    ; regExn0' "Empty"              Empty        (fn Empty        => ())
    ; regExn0' "OS.Path.InvalidArc" InvalidArc   (fn InvalidArc   => ())
    ; regExn0' "Match"              Match        (fn Match        => ())
    ; regExn0' "Option"             Option       (fn Option       => ())
    ; regExn0' "Overflow"           Overflow     (fn Overflow     => ())
    ; regExn0' "OS.Path.Path"       Path         (fn Path         => ())
    ; regExn0' "OS.IO.Poll"         Poll         (fn Poll         => ())
    ; regExn0' "Size"               Size         (fn Size         => ())
    ; regExn0' "Span"               Span         (fn Span         => ())
    ; regExn0' "Subscript"          Subscript    (fn Subscript    => ())
    ; regExn0' "Time.Time"          Time         (fn Time         => ())
    ; regExn0' "IEEEReal.Unordered" Unordered    (fn Unordered    => ())
    ; regExn1' "Fail" string        Fail         (fn Fail       ? =>  ?)
      (* Handlers for some extended-basis exceptions: *)
    ; regExn0' "IOSMonad.EOS"       IOSMonad.EOS (fn IOSMonad.EOS => ())
    ; regExn0' "Sum.Sum"            Sum.Sum      (fn Sum.Sum      => ())
    ; regExn0' "Fix.Fix"            Fix.Fix      (fn Fix.Fix      => ())
   end
end
