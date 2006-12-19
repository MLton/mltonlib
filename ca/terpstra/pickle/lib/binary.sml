structure Binary : PICKLER =
  struct
    exception Corrupt
    
    structure Base =
      struct
        type wt = Word8.word list
        type rt = Word8.word list
        type 'a r = rt -> rt * 'a
        type 'a w = wt * 'a -> wt
        
        val opts = { r = fn _ => fn (a :: r) => (r, Word8.toInt a) | _ => raise Corrupt,
                     w = fn _ => fn (l, i) => Word8.fromInt i :: l }
      end
    
    type t = Word8Vector.vector
    val eof = []
    val convert = { w = Word8Vector.fromList o rev,
                    r = fn v => List.tabulate 
                      (Word8Vector.length v, fn i => Word8Vector.sub (v, i)) }
    
    type unit = unit
    type bool = bool
    type char = char
    type word = word
    type int = int
    type string = string
    type 'a vector = 'a vector
    
    val unit = { r = fn l => (l, ()), w = fn (l, _) => l }
    val bool = { r = fn ((a:Word8.word) :: r) => (r, 0w0 <> a) | _ => raise Corrupt,
                 w = fn (l, b) => (if b then (0w1:Word8.word) else 0w0) :: l }
    val char = { r = fn (a :: r) => (r, Char.chr (Word8.toInt a)) | _ => raise Corrupt,
                 w = fn (l, c) => Word8.fromInt (Char.ord c) :: l }
    
    val (w2s, s2w) = (Word8.fromInt o Word.toInt, Word.fromInt o Word8.toInt)
    val (<<, >>, orb) = (Word.<<, Word.>>, Word.orb)
    infix 5 << >>
    infix 4 orb
    val word = { r = fn (w0 :: w1 :: w2 :: w3 :: r) => 
                     (r, s2w w0 << 0w24 orb s2w w1 << 0w16 orb s2w w2 << 0w8 orb s2w w3)
                     | _ => raise Corrupt,
                 w = fn (l, w) =>
                     w2s w :: w2s (w >> 0w8) :: w2s (w >> 0w16) :: w2s (w >> 0w24) :: l }
    val int = { r = fn l => let val (r, w) = #r word l in  (r, Word.toInt w) end,
                w = fn (l, i) => #w word (l, Word.fromInt i) }
    
    fun rstring l =
      let
        val (l, i) = #r int l
        val (s, l) = (List.take (l, i), List.drop (l, i))
        val s = List.map (Char.chr o Word8.toInt) s
        val s = implode s
      in
        (l, s)
      end
    fun wstring (l, s) =
        List.map (Word8.fromInt o Char.ord) (rev (explode s)) 
        @ #w int (l, String.size s)
    
    fun rvector f l = (l, Vector.fromList [])
    fun wvector f (l, v) = l (*!!! buggy *)
    
    val string = { r = rstring, w = wstring }
    val vector = { r = rvector, w = wvector }
  end
