functor Slice
   (S:
    sig
       type 'a elem

       structure Base: sig

          type 'a t

          val sub: 'a t * Int.t -> 'a elem
          val tabulate: Int.t * (Int.t -> 'a elem) -> 'a t
       end

       type 'a t

       val base: 'a t -> 'a Base.t * {start: Int.t}
       val full: 'a Base.t -> 'a t
       val size: 'a t -> Int.t
       val slice: 'a t * {size: Int.t, start:Int.t} -> 'a t
       val sub: 'a t * Int.t -> 'a elem
    end): SLICE = struct

   open S

   structure S = struct
      open S

      type 'a const = 'a Base.t * Int.t
      type 'a elem = 'a elem
      type 'a state = Int.t

      fun start s = let
         val (base, {start}) = base s
         val stop = start + size s
      in
         ((base, stop), start)
      end

      fun next ((base, stop), i) =
         if i = stop then
            None
         else
            Some (Base.sub (base, i), i + 1)
   end

   type 'a base = 'a Base.t

   local
      structure S = Enumerable (S)
   in
      open S
   end

   (* override definitions from Enumerable *)   
   local
      open S
   in
      val size = size
      val sub = sub
   end

   fun recurR (s, b, done, step) = let
      val (base, {start}) = base s
   in
      Util.recur
      ((start + size s, b), fn ((i, b), loop) => let
         val i = i - 1
      in
         if i < start then
            done b
         else
            step (Base.sub (base, i), b, fn b => loop (i, b))
      end)
   end

   fun isEmpty s = size s = 0

   fun dropPrefixN (s, n) = slice (s, {size = size s - n, start = n})

   fun dropSuffixN (s, n) = slice (s, {size = size s - n, start = 0})

   fun keepPrefixN (s, n) = dropSuffixN (s, size s - n)

   fun keepSuffixN (s, n) = dropPrefixN (s, size s - n)

   fun prefixSize (s, f) = 
      recur (s, 0, id, fn (x, n, k) => if f x then k (n + 1) else n)

   fun keepPrefix (s, f) = keepPrefixN (s, prefixSize (s, f))

   fun dropPrefix (s, f) = dropPrefixN (s, prefixSize (s, f))

   fun suffixSize (s, f) = 
      recurR (s, 0, id, fn (x, n, k) => if f x then k (n + 1) else n)

   fun dropSuffix (s, f) = dropSuffixN (s, suffixSize (s, f))

   fun keepSuffix (s, f) = keepSuffixN (s, suffixSize (s, f))

   fun splitPrefix (s, f) = let
      val n = prefixSize (s, f)
   in
      (keepPrefixN (s, n), dropPrefixN (s, n))
   end
      
   fun get s =
      if isEmpty s then
         None
      else
         Some (sub (s, 0), dropPrefixN (s, 1))
         
   fun map (s, f) = Base.tabulate (size s, fn i => f (sub (s, i)))

   local
      structure S =
         FieldsAndTokens
         (open S
          fun dropPrefix ((b, stop), i, f) = 
             Util.recur (i, fn (i, loop) =>
                         if i = stop then
                            i
                         else
                            if f (Base.sub (b, i)) then
                               loop (i + 1)
                            else
                               i)
          fun splitPrefix ((b, stop), i, f) = let
             val start = dropPrefix ((b, stop), i, f)
          in
             (slice (full b, {start = i, size = start - i}),
              start)
          end
             )
   in
      open S
   end

end
   
