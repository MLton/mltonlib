(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature DIR = sig

   structure Stream: sig
      type t

      val close: t -> Unit.t
      val read: t -> String.t Option.t
      val rewind: t -> Unit.t
   end

   type t = String.t

   val changeTo: t -> Unit.t
   val create: t -> Unit.t
   val current: Unit.t -> t
   val openStream: t -> Stream.t
   val remove: t -> Unit.t

end
