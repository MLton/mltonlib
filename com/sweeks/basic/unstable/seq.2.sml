structure Seq: SEQ = struct

   structure Seq = struct
      open Seq
   
      type 'a elem = 'a
   end

   open Seq

   local
      structure S = Get (Seq)
   in
      open S
   end

   val ofSeq = id

   val toSeq = id

   fun ofSeqN (s, _) = s

   fun single x = cons (x, empty ())

   type 'a unfold = unit

   type 'a unfoldR = 'a

   fun unfoldR (b, f) =
      Util.recur ((b, empty ()),  fn ((b, ac), loop) =>
                  case f b of
                     None => (ac, b)
                   | Some (a, b) => loop (b, cons (a, ac)))

   fun unfoldNR (n, b, f) =
      Util.recur
      ((n, b, empty ()), fn ((i, b, ac), loop) =>
       if i = 0 then
          (ac, b)
       else let
          val i = i - 1
       in
          case f (i, b) of
             None => die "unfoldNR"
           | Some (a, b) => loop (i, b, cons (a, ac))
       end)

   fun ofList l = #1 (unfold (l, fn [] => None | x :: l => Some (x, l)))

   fun ofListR l =
      Util.recur
      ((l, empty ()), fn ((l, ac), loop) =>
       case l of
          [] => ac
        | x :: l => loop (l, cons (x, ac)))

   fun reverse s = ofList (fold (s, [], op ::))

   val toSeqR = reverse
     
   fun tabulate (n, f) =
      #1 (unfold (0, fn i => if i = n then None else Some (f i, i + 1)))

   fun map (s, f) =
      #1 (unfold (s, fn s => Option.map (get s, fn (x, s) => (f x, s))))

   fun drop (s, f) =
      #1 (unfold
          (s, fn s =>
           Util.recur
           (s, fn (s, loop) =>
            case get s of
               None => None
             | Some (x, s) => if f x then loop s else Some (x, s))))

   fun keep (s, f) = drop (s, not o f)

   fun append (s, s') =
      delay
      (fn () =>
       case get s of
          None => s'
        | Some (x, s) => cons (x, append (s, s')))
            
   fun concat ss =
      delay
      (fn () =>
       case get ss of
          None => empty ()
        | Some (s, ss) => append (s, concat ss))

   fun separate (s, sep) =
      delay
      (fn () =>
       case get s of
          None => empty ()
        | Some (x, s) =>
             cons (x, concat (map (s, fn x => cons (sep, cons (x, empty ()))))))

   fun join (vs, sep) = concat (separate (vs, sep))

   fun keepPrefix (s, f) =
      #1 (unfold (s, fn s =>
                  case get s of
                     None => None
                   | Some (x, s) => if f x then Some (x, s) else None))

   fun keepPrefixN (s, n) =
      if n < 0 then
         die "takeN"
      else
         #1 (unfold ((s, n), fn (s, n) =>
                     if n = 0 then
                        None
                     else
                        case get s of
                           None => die "takeN"
                         | Some (x, s) => Some (x, (s, n - 1))))

   fun keepSuffix (s, f) = reverse (keepPrefix (reverse s, f))

   fun keepSuffixN (s, n) = dropPrefixN (s, size s - n)

   fun dropSuffix (s, f) = reverse (dropPrefix (reverse s, f))

   fun dropSuffixN (s, n) = keepPrefixN (s, size s - n)

   fun splitPrefix (s, f) =
      Util.recur
      ((s, []), fn ((s', ac), loop) =>
       case get s' of
          None => (s, empty ())
        | Some (x, s'') => 
             if f x then
                loop (s'', x :: ac)
             else
                (ofListR ac, s''))

   local
      structure S =
         FieldsAndTokens
         (open S
          val dropPrefix = fn ((), s, f) => dropPrefix (s, f)
          val splitPrefix = fn ((), s, f) => splitPrefix (s, f))
   in
      open S
   end
      
end

type 'a seq = 'a Seq.t
