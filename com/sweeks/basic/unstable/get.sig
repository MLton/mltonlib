signature GET = sig
   type 'a elem
   type 'a t
   val get: 'a t -> ('a elem * 'a t) option
end
