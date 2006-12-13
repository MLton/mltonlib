(* Copyright (C) 2006 Stephen Weeks.
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 *)
structure List: LIST = struct

   structure S = struct

      datatype t = datatype List.t

      type 'a elem = 'a

      val get = 
         fn [] => None
          | x :: l => Some (x, l)
   end

   structure S = struct
      open S
         
      local
         structure S = Get (S)
      in
         open S
      end
   end

   open S

   fun toSeqR l = fold (l, Seq.empty (), Seq.cons)
      
   fun reverse l = fold (l, [], op ::)

   fun ofSeqR s = Seq.fold (s, [], op ::)

   fun ofSeq s = reverse (ofSeqR s)

   fun ofSeqN (s, _) = ofSeq s
      
   fun empty () = []

   val cons = op ::

   fun single x = [x]

   type ('a, 'b) unfold = 'a * 'b

   type ('a, 'b) unfoldR = 'a * 'b

   fun unfoldNR (n, b, f) =
      Util.recur
      ((n, b, []), fn ((i, b, ac), loop) =>
       if i = 0 then
          (ac, b)
       else let
          val i = i - 1
       in
          case f (i, b) of
             None => die "unfoldNR"
           | Some (a, b) => loop (i, b, a :: ac)
       end)

   fun unfoldN (n, b, f) =
      Util.recur
      ((0, b, []), fn ((i, b, ac), loop) =>
       if i = n then
          (reverse ac, b)
       else
          case f (i, b) of
             None => die "unfoldN"
           | Some (a, b) => loop (i + 1, b, a :: ac))
      
   fun unfoldR (b, f) =
      Util.recur
      ((b, []),  fn ((b, ac), loop) =>
       case f b of
          None => (ac, b)
        | Some (a, b) => loop (b, a :: ac))

   fun unfold (b, f) = let
      val (s, b) =  unfoldR (b, f)
   in
      (reverse s, b)
   end

   fun tabulate (n, f) = #1 (unfoldNR (n, (), fn (x, ()) => Some (f x, ())))

   fun map (l, f) = reverse (fold (l, [], fn (x, ac) => f x :: ac))

   fun append (l, l') =
      if isEmpty l' then
         l
      else
         fold (l', reverse l, op ::)

   fun concat (ls: 'a t Seq.t): 'a t =
      fold (ofSeqR ls, [], append)

   fun separate (l, sep) =
      case l of
         [] => []
       | x :: l => fold (reverse l, [x], fn (x', l) => x' :: sep :: l)

   fun join (ls, sep) = concat (Seq.separate (ls, sep))
         
   fun keepPrefix (l, f) =
      reverse
      (recur (l, [], id, fn (x, ac, k) => if f x then k (x :: ac) else ac))

   fun keep (s, f) =
      fold (reverse s, [], fn (x, ac) => if f x then x :: ac else ac)

   fun drop (s, f) = keep (s, not o f)
            
   fun keepPrefixN (l, n) =
      if n < 0 then
         die "keepPrefixN"
      else
         reverse
         (recur (l, ([], n),
                 fn (ac, i) => if i = 0 then ac else die "keepPrefixN",
                 fn (x, (ac, i), k) =>
                 if i = 0 then ac else k (x :: ac, i - 1)))

   fun dropSuffix (s, f) =
      Util.recur (reverse s, fn (s, loop) =>
                  case s of
                     [] => []
                   | x :: s' => if f x then loop s' else reverse s)

   fun dropSuffixN (s, n) = keepPrefixN (s, size s - n)

   fun keepSuffix (s, f) = let
      val (size, n) =
         fold (s, (0, 0), fn (x, (n, k)) => (n + 1, if f x then k + 1 else 0))
   in
      dropPrefixN (s, size - n)
   end

   fun keepSuffixN (s, n) = dropPrefixN (s, size s - n)

   fun splitPrefix (s, f) =
      Util.recur
      ((s, []), fn ((s', ac), loop) =>
       case s' of
          [] => (s, [])
        | x :: s'' => 
             if f x then
                loop (s'', x :: ac)
             else
                (reverse ac, s'))

   local
      structure S =
         FieldsAndTokens
         (open S
          val dropPrefix = fn ((), s, f) => dropPrefix (s, f)
          val splitPrefix = fn ((), s, f) => splitPrefix (s, f))
   in
      open S
   end

   datatype t = datatype List.t

end

local
   open List
in
   val op @ = append
end
