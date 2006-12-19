signature PICKLER_BASE =
  sig
    type rt
    type wt
    type 'a r = rt -> rt * 'a
    type 'a w = wt * 'a -> wt
    val opts: { r: int -> int r, w: int -> int w }
  end

signature PICKLER_SIMPLE =
  sig
    structure Base : PICKLER_BASE
    type t
    
    val eof: Base.wt
    val convert: { r: t -> Base.rt, w: Base.wt -> t }
  end

signature PICKLER =
  sig
    include PICKLER_SIMPLE
    
    type unit = unit
    type bool = bool
    type char = char
    type word = word
    type int = int
    type string = string
    type 'a vector = 'a vector
    
    (* real, substring, exn, 'a array, 'a list, 'a ref, 'a array, order, 'a option *)
    val unit:   { r: unit   Base.r, w: unit   Base.w }
    val bool:   { r: bool   Base.r, w: bool   Base.w }
    val char:   { r: char   Base.r, w: char   Base.w }
    val word:   { r: word   Base.r, w: word   Base.w }
    val int:    { r: int    Base.r, w: int    Base.w }
    val string: { r: string Base.r, w: string Base.w }
    val vector: { r: 'a Base.r -> 'a vector Base.r, 
                  w: 'a Base.w -> 'a vector Base.w }
  end

signature PICKLE =
  sig
    structure Base : PICKLER_BASE
    
    type t
    type 'a pickle = { r: 'a Base.r, w: 'a Base.w }
    
    val compose1: { r: 'a Base.r -> 'b Base.r, w: 'a Base.w -> 'b Base.w } 
                  -> 'a pickle -> 'b pickle
    val compose2: { r: 'a1 Base.r * 'a2 Base.r -> 'b Base.r, 
                    w: 'a1 Base.w * 'a2 Base.w -> 'b Base.w } 
                  -> 'a1 pickle * 'a2 pickle -> 'b pickle
    
    val pickle: 'a pickle -> { r: t -> 'a,  w: 'a -> t }
  end
