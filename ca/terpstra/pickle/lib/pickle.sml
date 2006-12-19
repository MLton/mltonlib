functor Pickle(P : PICKLER_SIMPLE) :> PICKLE =
  struct
    structure Base = P.Base
    type 'a pickle = { r: 'a Base.r, w: 'a Base.w }
    type t = P.t
    
    fun compose1 {r=rf, w=wf} {r=r1, w=w1} = {r=rf r1, w=wf w1}
    fun compose2 {r=rf, w=wf} ({r=r1, w=w1}, {r=r2, w=w2}) = {r=rf(r1,r2), w=wf(w1,w2)}
    
    fun pickle ({r, w}: 'a pickle) =
      { r = fn t => (#2 o r o (#r P.convert)) t,
        w = fn x => (#w P.convert) (w (P.eof, x)) }
  end
      