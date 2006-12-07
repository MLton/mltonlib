functor FieldsAndTokens
   (S:
    sig
       include ENUMERATE

       val dropPrefix: 'a const * 'a state * ('a elem -> bool) -> 'a state
       val splitPrefix: 'a const * 'a state * ('a elem -> bool) -> 'a t * 'a state
    end):
   sig
      type 'a t
      type 'a elem
      val fields: 'a t * ('a elem -> bool) -> 'a t seq
      val tokens: 'a t * ('a elem -> bool) -> 'a t seq
   end = struct

   open S

   fun fields (s, f) = let
      val (c, s) = start s
   in
      Util.recur
      (s, fn (s, loop) =>
       Seq.delay
       (fn () => let
          val (x, s) = splitPrefix (c, s, not o f)
       in
          Seq.cons (x,
                    case next (c, s) of
                       None => Seq.empty ()
                     | Some (_, s) => loop s)
       end))
   end

   fun tokens (s, f) = let
      val (c, s) = start s
   in
      Util.recur
      (s, fn (s, loop) =>
       Seq.delay
       (fn () => let
          val s = dropPrefix (c, s, f)
       in
          if isNone (next (c, s)) then
             Seq.empty ()
          else let
             val (x, s) = splitPrefix (c, s, f)
          in
             Seq.cons (x, loop s)
          end
       end))
   end

end
