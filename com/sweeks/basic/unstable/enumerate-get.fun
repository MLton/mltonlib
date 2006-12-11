functor EnumerateGet (S: GET): ENUMERATE = struct

   open S
   type 'a const = Unit.t
   type 'a state = 'a t
   fun start s = ((), s)
   fun next ((), s) = get s
      
end
