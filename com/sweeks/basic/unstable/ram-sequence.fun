functor RamSequence
   (S: sig
          type 'a t

          val fromArray: 'a Array.t -> 'a t
          val size: 'a t -> Int.t
          val sub: 'a t * Int.t -> 'a
   end): SEQUENCE = struct

   open S

   structure S = struct
      open S

      type 'a const = 'a t
      type 'a elem = 'a
      type 'a state = Int.t
      fun start s = (s, 0)
      fun next (s, i) =
         if i = size s then
            None
         else
            Some (sub (s, i), i + 1)
   end

   local
      structure S = Enumerable (S)
   in
      open S
   end

   fun recurR (s, b, done, step) =
      Util.recur
      ((size s, b), fn ((i, b), loop) =>
       if i = 0 then
          done b
       else let
          val i = i - 1
       in
          step (sub (s, i), b, fn b => loop (i, b))
       end)

   fun empty () = fromArray (Array.Unsafe.make 0)

   fun toSeq v = Seq.map (Int.fromTo (0, size v), fn i => sub (v, i))

   fun toSeqR v = Seq.map (Int.fromToBy (size v, 0, ~1), fn i => sub (v, i))

   fun ofSeqN (s: 'a Seq.t, n) = let
      val a = Array.Unsafe.make n
      val n' = Seq.fold (s, 0, fn (x, i) =>
                         (Array.Unsafe.update (a, i, x); i + 1))
      val () =
         if n = n' then
            ()
         else
            die "RamSeq.ofSeqN"
   in
      S.fromArray a
   end

   fun ofSeq s = ofSeqN (s, Seq.size s)

   type ('a, 'b) unfold = 'a * 'b

   type ('a, 'b) unfoldR = 'a * 'b

   local
      fun make fold (n, b, f) = let
         val a = Array.Unsafe.make n
         val b =
            fold
            (0, n, b, fn (i, b) =>
             case f (i, b) of
                None => die "unfoldN"
              | Some (x, b) => (Array.Unsafe.update (a, i, x); b))
      in
         (fromArray a, b)
      end
   in
      fun unfoldN ? = make Int.fold ?
      fun unfoldNR ? = make Int.foldDown ?
   end

   fun ofList l =
      #1 (unfoldN
          (List.size l, l, fn (_, []) => None | (_, x :: l) => Some (x, l)))

   local
      fun make lf (b, f) = let
         val (l, b) = lf (b, f)
      in
         (ofList l, b)
      end
   in
      val unfold = fn ? => make List.unfold ?
      val unfoldR = fn ? => make List.unfoldR ?
   end

   fun append (s1, s2) = let
      val n1 = size s1
      val n2 = size s2
      val n = n1 + n2
      val a = Array.Unsafe.make n
      val _ = Int.for (0, n1, fn i =>
                       Array.Unsafe.update (a, i, sub (s1, i)))
      val _ = Int.for (n1, n, fn i =>
                       Array.Unsafe.update (a, i, sub (s2, i - n1)))
   in
      fromArray a
   end

   fun tabulate (n, f) = let
      val a = Array.Unsafe.make n
      val _ = Int.for (0, n, fn i => Array.Unsafe.update (a, i, f i))
   in
      fromArray a
   end

   fun single x = tabulate (1, const x)

   fun map (s, f) = tabulate (size s, fn i => f (sub (s, i)))

   fun cons (x, s) =
      tabulate (size s + 1, fn i => if i = 0 then x else sub (s, i + 1))

   fun isEmpty s = size s = 0

   fun separate (s, x) =
      if isEmpty s then
         empty ()
      else let
         val a = Array.Unsafe.make (2 * size s - 1)
         val () = Array.Unsafe.update (a, 0, sub (s, 0))
         val () =
            Int.for (1, size s, fn i =>
                     let
                        val i = 2 * i
                     in
                        Array.Unsafe.update (a, i - 1, x);
                        Array.Unsafe.update (a, i, sub (s, i))
                     end)
      in
         fromArray a
      end

   fun join (ss, sep) =
      case Seq.get ss of
         None => empty ()
       | Some (s, ss') => let
            val n =
               Seq.fold (ss', size s + size sep * Seq.size ss',
                         fn (s, n) => n + size s)
            val a = Array.Unsafe.make n
            fun blat (s, at) =
               (Int.for (0, size s, fn i =>
                         Array.update (a, at + i, sub (s, i)));
                at + size s)
            val at = 0
            val at = blat (s, at)
            val _ = Seq.fold (ss', at, fn (s, at) => blat (s, blat (sep, at)))
         in
            S.fromArray a
         end
      
   fun concat ss = join (ss, empty ())

   fun last s = sub (s, size s - 1)

   fun dropPrefixN (s, n) = tabulate (size s - n, fn i => sub (s, n + i))

   fun dropPrefix (s, f) =
      recur (s, 0, fn _ => empty (),
             fn (x, n, k) => if f x then k (n + 1) else dropPrefixN (s, n))

   fun dropSuffixN (s, n) = tabulate (size s - n, fn i => sub (s, i))

   fun suffixSize (s, f) =
      recurR (s, 0, id, fn (x, n, k) => if f x then k (n + 1) else n)

   fun dropSuffix (s, f) = dropSuffixN (s, suffixSize (s, f))

   fun keepPrefixN (s, n) = tabulate (n, fn i => sub (s, i))

   fun keepPrefix (s, f) =
      keepPrefixN (s, recur (s, 0, id, fn (x, n, k) =>
                             if f x then k (n + 1) else n))

   fun keepSuffixN (s, n) = dropPrefixN (s, size s - n)

   fun keepSuffix (s, f) = keepSuffixN (s, suffixSize (s, f))

   fun reverse s = let
      val n = size s
      val n1 = n - 1
   in
      tabulate (n, fn i => sub (s, n1 - i))
   end

   fun splitPrefix (s, f) = let
      val i =
         Util.recur
         (0, fn (i, loop) =>
          if i = size s orelse not (f (sub (s, i))) then
             i
          else
             loop (i + 1))
   in
      (keepPrefixN (s, i), dropPrefixN (s, i))
   end

   fun keep (s, f) = let
      val s = map (s, fn x => if f x then Some x else None)
   in
      #1 (unfoldN (fold (s, 0, fn (x, i) => if isSome x then i + 1 else i),
                   0,
                   fn (_, i) =>
                   Util.recur (i, fn (i, loop) =>
                               case sub (s, i) of
                                  None => loop (i + 1)
                                | Some x => Some (x, i + 1))))
   end

   fun drop (s, f) = keep (s, not o f)

   local
      structure S =
         FieldsAndTokens
         (open S
          fun dropPrefix (s, i, f) =
             recur (s, i, id,
                    fn (x, n, k) => if f x then k (n + 1) else n)
          fun splitPrefix (s, i, f) = let
             val n =
                Util.recur
                (i, fn (i, loop) =>
                 if i = size s orelse not (f (sub (s, i))) then
                    i
                 else
                    loop (i + 1))
          in
             (tabulate (n, fn j => sub (s, i + j)),
              i + n)
          end)
   in
      open S
   end

end
