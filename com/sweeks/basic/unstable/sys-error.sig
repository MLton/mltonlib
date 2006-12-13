(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
signature SYS_ERROR = sig
   
   structure Exn: sig
      type t
      type error

      exception E of t

      val message: t -> String.t
      val error: t -> error Option.t
   end
   
   type t
   sharing type t = Exn.error
      
   val == : t * t -> Bool.t
   val message: t -> String.t
   val name: t -> String.t
   val ofName: String.t -> t Option.t

end
