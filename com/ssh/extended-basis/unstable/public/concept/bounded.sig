(* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)

(** == Bounded == *)

signature BOUNDED_CORE = sig
   type bounded
   val bounds : bounded Sq.t
end

signature BOUNDED_EX = sig
   type bounded_ex
   val maxValue : bounded_ex
   val minValue : bounded_ex
end

signature BOUNDED = sig
   include BOUNDED_CORE BOUNDED_EX
   sharing type bounded = bounded_ex
end

(** == Maybe Bounded == *)

signature MAYBE_BOUNDED_CORE = sig
   type bounded
   val bounds : bounded Sq.t Option.t
end

signature MAYBE_BOUNDED_EX = sig
   type bounded_ex
   val maxValue : bounded_ex Option.t
   val minValue : bounded_ex Option.t
end

signature MAYBE_BOUNDED = sig
   include MAYBE_BOUNDED_CORE MAYBE_BOUNDED_EX
   sharing type bounded = bounded_ex
end
