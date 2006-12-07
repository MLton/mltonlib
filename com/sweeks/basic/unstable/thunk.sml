structure Thunk: THUNK = struct

   type 'a t = unit -> 'a

end

type 'a thunk = 'a Thunk.t
