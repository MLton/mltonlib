(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature IO_DESC = sig

   structure Kind: sig
      type t

      val == : t * t -> Bool.t
      val device: t
      val dir: t
      val file: t
      val pipe: t
      val socket: t
      val symlink: t
      val tty: t
   end

   type t

   val hash: t -> Word.t
   val compare: t * t -> Order.t

end
