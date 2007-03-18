(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
open Export

datatype z = datatype Bool.t
val _: z = false (* quell unused warning *)
datatype z = datatype List.t
val _: Unit.t z = [] (* quell unused warning *)
datatype z = datatype Order.t
val _: z = Less (* quell unused warning *)
datatype z = datatype Option.t
val _: Unit.t z = None (* quell unused warning *)
datatype z = datatype Ref.t
type z = Unit.t

(* The following are so that nice type names are used by -show-basis. *)
structure Int = Int
structure IntInf = IntInf
structure Real = Real
structure Word = Word
