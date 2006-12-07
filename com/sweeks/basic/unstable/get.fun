functor Get
   (S:
    sig
       type 'a elem
       type 'a t
       val get: 'a t -> ('a elem * 'a t) option
    end) = struct

   structure S = struct
      open S
      
      local
         structure S = EnumerateGet (S)
      in
         open S
      end
   end

   open S

   local
      structure S = Enumerable (S)
   in
      open S
   end

   fun dropPrefix (s, f) =
      Util.recur (s, fn (s, loop) =>
                  case get s of
                     None => s
                   | Some (x, s') => if f x then loop s' else s)

   fun dropPrefixN (s, n) =
      if n < 0 then
         die "dropPrefixN"
      else
         Util.recur ((s, n), fn ((s, n), loop) =>
                     if n = 0 then
                        s
                     else
                        case get s of
                           None => die "dropPrefixN"
                         | Some (_, s) => loop (s, n - 1))

end
