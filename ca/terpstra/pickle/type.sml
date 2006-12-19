fun dtype ((data, bind), (l, scope)) =
  let
    val withscope = Map.union (scope, defd data)
    val datascope = Map.union (withscope, defd bind)
    
    fun dump scope =
      let
        fun record (n, typ) l = l && n && ": " ++ inline typ
        and tyvars v l =
          if Vector.length v = 0 then l else
          l && "(" ++ sfoldl (inline o #1, ", ", v) && ") "
        and invoke n l = 
          case Map.fetch scope n of
              NONE => l && "Arg." && n
            | SOME _ =>  l && n
        and inline node l =
          case node of
              (RECORD v) => l && "{ " ++ sfoldl (record, ", ", v) && " }"
            | (RECURSIVE (n, v, _)) => l ++ tyvars v ++ invoke n
            | (TUPLE v) => l ++ sfoldl (inline, " * ", v)
            | (TYVAR (tyvar, _, _)) => l && tyvar
      in
        inline
      end
    
    fun withtyp (i, { name, reader=_, writer=_, tyvars, typ }) l =
      l && (if i = 0 then "type " else "and ")
      ++ tuple (#1, tyvars) && name && " = " ++ dump withscope typ && "\n"
    
    fun const (n, NONE) l = l && n && "\n"
      | const (n, SOME typ) l = l && n && " of " ++ dump datascope typ && "\n"
    fun datatyp (i, { name, reader=_, writer=_, tyvars, typ = (_, _, v) }) l =
      l && (if i = 0 then "    datatype " else "    and ")
      ++ tuple (#1, tyvars) && name &&  " = "  ++ sfoldl (const, "\t| ", v)
  in
    (l ++ foldli (datatyp, data)
     && (if Vector.length data <> 0 andalso 
            Vector.length bind <> 0 then "with" else "")
     ++ foldli (withtyp, bind),
     datascope)
  end
