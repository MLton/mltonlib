functor EnumerateGet (S: GET): ENUMERATE = struct

   open S
   type 'a const = unit
   type 'a state = 'a t
   fun start s = ((), s)
   fun next ((), s) = get s
      
end
