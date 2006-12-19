local
  fun record (n, typ) l = l && n && "=" ++ inline typ
  and inline node l =
    case node of
        (RECORD v) => 
          l && "{ " ++ sfoldl (record, ", ", v) && " }"
      | (RECURSIVE (_, _, t)) => l && t
      | (TUPLE v) => 
          l && "(" ++ sfoldl (inline, ", ", v) && ")"
      | (TYVAR (_, t, _)) => l && t
in
  val pattern = inline
end

fun method start (arg as ((data, bind), (l, scope))) =
  let
    val withscope = Map.union (scope,     defd data)
    val datascope = Map.union (withscope, defd bind)
    exception Undefined
    
    fun mapOpt (f, NONE) l = l
      | mapOpt (f, SOME x) l = l ++ f x
      
    fun data_opts { typ = (ropt, wopt, v), ... } l =
      l && "val (" && ropt && ", " && wopt && ") = ((#r Base.opts) "
      && Int.toString (Vector.length v) && ", (#w Base.opts) "
      && Int.toString (Vector.length v) && ")\n"
    
    fun write_inline scope =
      let
        fun fnhelper (typ, f) l =
          l && "    fun " && f && " (a, " ++ pattern typ && ") =\n"
          && "      let\n    val a = a\n" ++ inline typ 
          && "      in\n        a\n      end\n"
        and invoke n l =
          case Map.fetch scope n of
              NONE => l && "(#w Arg." && n && ")"
            | (SOME {reader=_, writer}) => l && writer
        and inline node l =
          case node of
              (RECORD v) => l ++ foldl (inline o #2, v)
            | (TUPLE v) => l ++ foldl (inline, v)
            | (TYVAR (_, t, f)) => 
                l && "    val a = " && f && " (a, " && t && ")\n"
            | (RECURSIVE (n, v, t)) =>
                l ++ foldl (fnhelper, v) && "    val a = "
                ++ invoke n && " "
                ++ tuple (#2, v) && "(a, " && t && ")\n"
      in
        inline
      end
    
    fun write_bind { name=_, reader=_, writer, tyvars, typ } l = 
      l && "and " && writer ++ tuple (#2, tyvars) 
      && " (a, " ++ pattern typ && ") =\n" 
      && "  let\n    val a = a\n" ++ write_inline withscope typ 
      && "  in\n    a\n  end\n"
    
    fun write_data { name=_, reader=_, writer, tyvars, typ = (_, wopt, v) } l =
      let
        fun constr (opt, (constr, typo)) l =
          l && (if opt = 0 then "  " else "| ") 
          && "(" && constr && " " ++ mapOpt (pattern, typo) && ") =>\n"
          && "  let\n    val a = " && wopt && " (a, " && Int.toString opt && ")\n" 
          ++ mapOpt (write_inline datascope, typo) && "  in\n    a\n  end\n"
      in
        l && "and " && writer && " " ++ tuple (#2, tyvars)
        && "(a, x) = case x of\n" 
        ++ foldli (constr, v)
      end
    
    fun read_inline scope =
      let
        fun fnhelper (typ, f) l =
          l && "    fun " && f && " a =\n      let\n" 
          && "    val a = a\n" ++ inline typ && "      in\n        (a, "
          ++ pattern typ && ")\n      end\n"
        and invoke n l =
          case Map.fetch scope n of
              NONE => l && "(#r Arg." && n && ")"
            | (SOME {reader, writer=_}) => l && reader
        and inline node l =
          case node of
              (RECORD v) => l ++ foldl (inline o #2, v)
            | (TUPLE v) => l ++ foldl (inline, v)
            | (TYVAR (_, t, f)) => 
                l && "    val (a, " && t && ") = " && f && " a\n"
            | (RECURSIVE (n, v, t)) =>
                l ++ foldl (fnhelper, v) && "    val (a, " && t 
                && ") = " ++ invoke n && " " ++ tuple (#2, v) && "a\n"
      in
        inline
      end
      
    fun read_bind { name=_, reader, writer=_, tyvars, typ } l =
      l && "and " && reader && " " ++ tuple (#2, tyvars) && "a =\n"
      && " let\n    val a = a\n" ++ read_inline withscope typ && "  in\n"
      && "    (a, " ++ pattern typ && ")\n  end\n" 
    
    fun read_data { name=_, reader, writer=_, tyvars, typ = (ropt, _, v) } l =
      let
        fun constr (opt, (constr, typo)) l =
          l && (if opt = 0 then "  " else "| ")
          && "(a, " && Int.toString opt && ") =>\n"
          && "  let\n    val a = a\n"
          ++ mapOpt (read_inline datascope, typo) 
          && "  in\n    (a, " && constr && " "
          ++ mapOpt (pattern, typo) && ")\n  end\n"
      in
        l && "and " && reader && " " ++ tuple (#2, tyvars) && "a =\n"
        && "case " && ropt && " a of\n" 
        ++ foldli (constr, v) && "| (a, _) => raise Corrupt\n"
      end
      
      fun binds {name, reader, writer, tyvars=_, typ=_} l =
        l && "val " && name &&  " = "
        && " { r = " && reader && ", w = " && writer && " }\n"
    in
      (#1 (dtype arg)
       ++ foldl (data_opts, data)
       && "fun " && start && " x = x\n"
       ++ foldl (write_bind, bind) ++ foldl (write_data, data)
       ++ foldl ( read_bind, bind) ++ foldl ( read_data, data)
       ++ foldl (binds, bind) ++ foldl (binds, data),
       datascope)
    end
