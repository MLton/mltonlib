structure Thunk: THUNK = struct

   type 'a t = Unit.t -> 'a

end

type 'a thunk = 'a Thunk.t
