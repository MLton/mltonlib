(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure Option = struct

   datatype 'a t = None | Some of 'a

   fun map (opt, f) =
      case opt of
         None => None
       | Some x => Some (f x)

   val valOf =
      fn None => raise Option
       | Some x => x

   val isNone = fn None => true | _ => false

   val isSome = fn Some _ => true | _ => false

   local
      datatype z = datatype Basis.Option.option
   in
      val ofBasis = fn NONE => None | SOME x => Some x
      val toBasis = fn None => NONE | Some x => SOME x
   end

end

local
   open Option
in
   datatype z = datatype t
   val isNone = isNone
   val isSome = isSome
   val valOf = valOf
end
