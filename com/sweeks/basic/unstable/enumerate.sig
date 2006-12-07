signature ENUMERATE = sig
   type 'a const
   type 'a elem
   type 'a state
   type 'a t
   val start: 'a t -> 'a const * 'a state
   val next: 'a const * 'a state -> ('a elem * 'a state) option
end
